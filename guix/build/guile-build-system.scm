;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build guile-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix build utils)
  #:export (target-guile-effective-version
            %standard-phases
            guile-build))

(define* (target-guile-effective-version #:optional guile)
  "Return the effective version of GUILE or whichever 'guile' is in $PATH.
Return #false if it cannot be determined."
  (let* ((pipe (open-pipe* OPEN_READ
                           (if guile
                               (string-append guile "/bin/guile")
                               "guile")
                           "-c" "(display (effective-version))"))
         (line (read-line pipe)))
    (and (zero? (close-pipe pipe))
         (string? line)
         line)))

(define (file-sans-extension file)                ;TODO: factorize
  "Return the substring of FILE without its extension, if any."
  (let ((dot (string-rindex file #\.)))
    (if dot
        (substring file 0 dot)
        file)))

(define %scheme-file-regexp
  ;; Regexp to match Scheme files.
  "\\.(scm|sls)$")

(define %documentation-file-regexp
  ;; Regexp to match README files and the likes.
  "^(README.*|.*\\.html|.*\\.org|.*\\.md)$")

(define* (set-locale-path #:key inputs native-inputs
                          #:allow-other-keys)
  "Set 'GUIX_LOCPATH'."
  (match (assoc-ref (or native-inputs inputs) "locales")
    (#f #t)
    (locales
     (setenv "GUIX_LOCPATH" (string-append locales "/lib/locale"))
     #t)))

(define* (build #:key outputs inputs native-inputs
                (source-directory ".")
                (compile-flags '())
                (scheme-file-regexp %scheme-file-regexp)
                target
                #:allow-other-keys)
  "Build files in SOURCE-DIRECTORY that match SCHEME-FILE-REGEXP."
  (let* ((out        (assoc-ref outputs "out"))
         (guile      (assoc-ref (or native-inputs inputs) "guile"))
         (effective  (target-guile-effective-version guile))
         (module-dir (string-append out "/share/guile/site/"
                                    effective))
         (go-dir     (string-append out "/lib/guile/"
                                    effective "/site-ccache/"))
         (guild      (string-append guile "/bin/guild"))
         (flags      (if target
                         (cons (string-append "--target=" target)
                               compile-flags)
                         compile-flags)))
    (if target
        (format #t "Cross-compiling for '~a' with Guile ~a...~%"
                target effective)
        (format #t "Compiling with Guile ~a...~%" effective))
    (format #t "compile flags: ~s~%" flags)

    ;; Make installation directories.
    (mkdir-p module-dir)
    (mkdir-p go-dir)

    ;; Compile .scm files and install.
    (setenv "GUILE_AUTO_COMPILE" "0")
    (setenv "GUILE_LOAD_COMPILED_PATH"
            (string-append go-dir
                           (match (getenv "GUILE_LOAD_COMPILED_PATH")
                             (#f "")
                             (path (string-append ":" path)))))
    (for-each (lambda (file)
                (let* ((go (string-append go-dir
                                          (file-sans-extension file)
                                          ".go")))
                  ;; Install source module.
                  (install-file (string-append source-directory "/" file)
                                (string-append module-dir
                                               "/" (dirname file)))

                  ;; Install and compile module.
                  (apply invoke guild "compile" "-L" source-directory
                         "-o" go
                         (string-append source-directory "/" file)
                         flags)))

              ;; Arrange to strip SOURCE-DIRECTORY from file names.
              (with-directory-excursion source-directory
                (find-files "." scheme-file-regexp)))
    #t))

(define* (install-documentation #:key outputs
                                (documentation-file-regexp
                                 %documentation-file-regexp)
                                #:allow-other-keys)
  "Install files that mactch DOCUMENTATION-FILE-REGEXP."
  (let* ((out (assoc-ref outputs "out"))
         (doc (string-append out "/share/doc/"
                             (strip-store-file-name out))))
    (for-each (cut install-file <> doc)
              (find-files "." documentation-file-regexp))
    #t))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (add-before 'install-locale 'set-locale-path
      set-locale-path)
    (replace 'build build)
    (add-after 'build 'install-documentation
      install-documentation)
    (delete 'check)
    (delete 'strip)
    (delete 'validate-runpath)
    (delete 'install)))

(define* (guile-build #:key (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Guile package, applying all of PHASES in order."
  (apply gnu:gnu-build #:phases phases args))
