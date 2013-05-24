;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build gnu-cross-build)
  #:use-module (guix build utils)
  #:use-module ((guix build gnu-build-system)
                #:renamer (symbol-prefix-proc 'build:))
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%standard-cross-phases
            gnu-cross-build))

;;; Commentary:
;;;
;;; Extension of `gnu-build-system.scm' to support cross-compilation.
;;;
;;; Code:

(define* (set-paths #:key inputs native-inputs
                    (search-paths '()) (native-search-paths '())
                    #:allow-other-keys)
  (define input-directories
    (match inputs
      (((_ . dir) ...)
       dir)))

  (define native-input-directories
    (match native-inputs
      (((_ . dir) ...)
       dir)))

  ;; $PATH must refer only to native (host) inputs since target inputs are not
  ;; executable.
  (set-path-environment-variable "PATH" '("bin" "sbin")
                                 native-input-directories)

  ;; Search paths for target inputs.
  (for-each (match-lambda
             ((env-var (directories ...) separator)
              (set-path-environment-variable env-var directories
                                             input-directories
                                             #:separator separator)))
            search-paths)

  ;; Search paths for native inputs.
  (for-each (match-lambda
             ((env-var (directories ...) separator)
              (set-path-environment-variable env-var directories
                                             native-input-directories
                                             #:separator separator)))
            native-search-paths)

  ;; Dump the environment variables as a shell script, for handy debugging.
  (system "export > environment-variables"))

(define* (configure #:key
                    inputs outputs (configure-flags '()) out-of-source?
                    target native-inputs
                    #:allow-other-keys)
  (format #t "configuring for cross-compilation to `~a'~%" target)
  (apply (assoc-ref build:%standard-phases 'configure)
         #:configure-flags (cons (string-append "--host=" target)
                                 configure-flags)

         ;; XXX: The underlying `configure' phase looks for Bash among
         ;; #:inputs, so fool it this way.
         #:inputs native-inputs

         #:outputs outputs
         #:out-of-source? out-of-source?
         '()))

(define* (strip #:key target outputs (strip-binaries? #t)
                (strip-flags '("--strip-debug"))
                (strip-directories '("lib" "lib64" "libexec"
                                     "bin" "sbin"))
                #:allow-other-keys)
  ;; TODO: The only difference with `strip' in gnu-build-system.scm is the
  ;; name of the strip command; factorize it.

  (define (strip-dir dir)
    (format #t "stripping binaries in ~s with flags ~s~%"
            dir strip-flags)
    (file-system-fold (const #t)
                      (lambda (path stat result)  ; leaf
                        (zero? (apply system*
                                      (string-append target "-strip")
                                      (append strip-flags (list path)))))
                      (const #t)                  ; down
                      (const #t)                  ; up
                      (const #t)                  ; skip
                      (lambda (path stat errno result)
                        (format (current-error-port)
                                "strip: failed to access `~a': ~a~%"
                                path (strerror errno))
                        #f)
                      #t
                      dir))

  (or (not strip-binaries?)
      (every strip-dir
             (append-map (match-lambda
                          ((_ . dir)
                           (filter-map (lambda (d)
                                         (let ((sub (string-append dir "/" d)))
                                           (and (directory-exists? sub) sub)))
                                       strip-directories)))
                         outputs))))

(define %standard-cross-phases
  ;; The standard phases when cross-building.
  (let ((replacements `((set-paths ,set-paths)
                        (configure ,configure)
                        (strip ,strip))))
    (fold (lambda (replacement phases)
            (match replacement
              ((name proc)
               (alist-replace name proc phases))))
          (alist-delete 'check build:%standard-phases)
          replacements)))

;;; gnu-cross-build.scm ends here
