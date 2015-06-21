;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
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
  #:use-module (guix build utils)
  #:use-module (guix build emacs-utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:export (%standard-phases
            emacs-build))

;; Commentary:
;;
;; Builder-side code of the build procedure for ELPA Emacs packages.
;;
;; Code:

;; Directory suffix where we install ELPA packages.  We avoid ".../elpa" as
;; Emacs expects to find the ELPA repository 'archive-contents' file and the
;; archive signature.
(define %install-suffix "/share/emacs/site-lisp/guix.d")

(define* (build #:key outputs inputs #:allow-other-keys)
  "Compile .el files."
  (let* ((emacs (string-append (assoc-ref inputs "emacs") "/bin/emacs"))
         (out (assoc-ref outputs "out"))
         (elpa-name-ver (store-directory->elpa-name-version out))
         (el-dir (string-append out %install-suffix "/" elpa-name-ver))
         (deps-dirs (emacs-inputs-directories inputs)))
    (setenv "SHELL" "sh")
    (parameterize ((%emacs emacs))
      (emacs-byte-compile-directory el-dir
                                    (emacs-inputs-el-directories deps-dirs)))))

(define* (patch-el-files #:key outputs #:allow-other-keys)
  "Substitute the absolute \"/bin/\" directory with the right location in the
store in '.el' files."
  (let* ((out (assoc-ref outputs "out"))
         (elpa-name-ver (store-directory->elpa-name-version out))
         (el-dir (string-append out %install-suffix "/" elpa-name-ver))
         (substitute-cmd (lambda ()
                           (substitute* (find-files "." "\\.el$")
                             (("\"/bin/(.*)\"" _ cmd)
                              (string-append "\"" (which cmd) "\""))))))
    (with-directory-excursion el-dir
      ;; Some old '.el' files (e.g., tex-buf.el in AUCTeX) are still encoded
      ;; with the "ISO-8859-1" locale.
      (unless (false-if-exception (substitute-cmd))
        (with-fluids ((%default-port-encoding "ISO-8859-1"))
          (substitute-cmd))))
    #t))

(define* (install #:key outputs #:allow-other-keys)
  "Install the package contents."
  (let* ((out (assoc-ref outputs "out"))
         (elpa-name-ver (store-directory->elpa-name-version out))
         (src-dir (getcwd))
         (tgt-dir (string-append out %install-suffix "/" elpa-name-ver)))
    (copy-recursively src-dir tgt-dir)
    #t))

(define* (move-doc #:key outputs #:allow-other-keys)
  "Move info files from the ELPA package directory to the info directory."
  (let* ((out (assoc-ref outputs "out"))
         (elpa-name-ver (store-directory->elpa-name-version out))
         (el-dir (string-append out %install-suffix "/" elpa-name-ver))
         (name-ver (store-directory->name-version out))
         (info-dir (string-append out "/share/info/" name-ver))
         (info-files (find-files el-dir "\\.info$")))
    (unless (null? info-files)
      (mkdir-p info-dir)
      (with-directory-excursion el-dir
        (when (file-exists? "dir") (delete-file "dir"))
        (for-each (lambda (f)
                    (copy-file f (string-append info-dir "/" (basename f)))
                    (delete-file f))
                  info-files)))
    #t))

(define* (make-autoloads #:key outputs inputs #:allow-other-keys)
  "Generate the autoloads file."
  (let* ((emacs (string-append (assoc-ref inputs "emacs") "/bin/emacs"))
         (out (assoc-ref outputs "out"))
         (elpa-name-ver (store-directory->elpa-name-version out))
         (elpa-name (package-name->name+version elpa-name-ver))
         (el-dir (string-append out %install-suffix "/" elpa-name-ver)))
    (parameterize ((%emacs emacs))
      (emacs-generate-autoloads elpa-name el-dir))
    #t))

(define (emacs-package? name)
  "Check if NAME correspond to the name of an Emacs package."
  (string-prefix? "emacs-" name))

(define (emacs-inputs inputs)
  "Retrieve the list of Emacs packages from INPUTS."
  (filter (match-lambda
            ((label directory)
             (emacs-package? ((compose package-name->name+version
                                       store-directory->name-version)
                              directory)))
            (_ #f))
          inputs))

(define (emacs-inputs-directories inputs)
  "Extract the list of Emacs package directories from INPUTS."
  (let ((inputs (emacs-inputs inputs)))
    (match inputs
      (((names . directories) ...) directories))))

(define (emacs-inputs-el-directories dirs)
  "Build the list of Emacs Lisp directories from the Emacs package directory
DIRS."
  (map (lambda (d)
         (string-append d %install-suffix "/"
                        (store-directory->elpa-name-version d)))
       dirs))

(define (package-name-version->elpa-name-version name-ver)
  "Convert the Guix package NAME-VER to the corresponding ELPA name-version
format.  Essnetially drop the prefix used in Guix."
  (let ((name (store-directory->name-version name-ver)))
    (if (emacs-package? name-ver)
        (store-directory->name-version name-ver)
        name-ver)))

(define (store-directory->elpa-name-version store-dir)
  "Given a store directory STORE-DIR return the part of the basename after the
second hyphen.  This corresponds to 'name-version' as used in ELPA packages."
  ((compose package-name-version->elpa-name-version
            store-directory->name-version)
   store-dir))

(define (store-directory->name-version store-dir)
  "Given a store directory STORE-DIR return the part of the basename
after the first hyphen.  This corresponds to 'name-version' of the package."
  (let* ((base (basename store-dir)))
    (string-drop base
                 (+ 1 (string-index base #\-)))))

;; from (guix utils).  Should we put it in (guix build utils)?
(define (package-name->name+version name)
  "Given NAME, a package name like \"foo-0.9.1b\", return two values:
\"foo\" and \"0.9.1b\".  When the version part is unavailable, NAME and
#f are returned.  The first hyphen followed by a digit is considered to
introduce the version part."
  ;; See also `DrvName' in Nix.

  (define number?
    (cut char-set-contains? char-set:digit <>))

  (let loop ((chars   (string->list name))
             (prefix '()))
    (match chars
      (()
       (values name #f))
      ((#\- (? number? n) rest ...)
       (values (list->string (reverse prefix))
               (list->string (cons n rest))))
      ((head tail ...)
       (loop tail (cons head prefix))))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'configure)
    (delete 'check)
    (delete 'install)
    (replace 'build build)
    (add-before 'build 'install install)
    (add-after 'install 'make-autoloads make-autoloads)
    (add-after 'make-autoloads 'patch-el-files patch-el-files)
    (add-after 'make-autoloads 'move-doc move-doc)))

(define* (emacs-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Emacs package, applying all of PHASES in order."
  (apply gnu:gnu-build
         #:inputs inputs #:phases phases
         args))

;;; emacs-build-system.scm ends here
