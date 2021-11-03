;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (guix build meson-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
  #:use-module (guix build utils)
  #:use-module (guix build gremlin)
  #:use-module (guix elf)
  #:use-module (ice-9 match)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:export (%standard-phases
            meson-build))

;; Commentary:
;;
;; Builder-side code of the standard meson build procedure.
;;
;; Code:

(define* (configure #:key outputs configure-flags build-type
                    #:allow-other-keys)
  "Configure the given package."
  (let* ((out (assoc-ref outputs "out"))
         (source-dir (getcwd))
         (build-dir "../build")
         (prefix (assoc-ref outputs "out"))
         (args `(,(string-append "--prefix=" prefix)
                 ,(string-append "--buildtype=" build-type)
                 ,(string-append "-Dc_link_args=-Wl,-rpath="
                                 (assoc-ref outputs "out") "/lib")
                 ,(string-append "-Dcpp_link_args=-Wl,-rpath="
                                 (assoc-ref outputs "out") "/lib")
                 ,@configure-flags
                 ,source-dir)))

    (mkdir build-dir)
    (chdir build-dir)
    (apply invoke "meson" args)))

(define* (build #:key parallel-build?
                #:allow-other-keys)
  "Build a given meson package."
  (invoke "ninja" "-j" (if parallel-build?
                           (number->string (parallel-job-count))
                           "1")))

(define* (check #:key tests? test-options parallel-tests?
                #:allow-other-keys)
  (if tests?
      (begin
        (setenv "MESON_TESTTHREADS"
                (if parallel-tests?
                    (number->string (parallel-job-count))
                    "1"))
        ;; Always provide "-t 0" to disable the 30 s default timeout.
        (apply invoke "meson" "test" "--print-errorlogs" "-t" "0" test-options))
      (format #t "test suite not run~%")))

(define* (install #:rest args)
  (invoke "ninja" "install"))

(define* (shrink-runpath #:key (elf-directories '("lib" "lib64" "libexec"
                                                  "bin" "sbin"))
                         outputs #:allow-other-keys)
  "Go through all ELF files from ELF-DIRECTORIES and shrink the RUNPATH
since a lot of directories are left over from the build phase of meson,
for example libraries only needed for the tests."

  (define handle-output
    (match-lambda
      ((output . directory)
       (let* ((elf-dirnames (map (lambda (subdir)
                                   (string-append directory "/" subdir))
                                 elf-directories))
              (existing-elf-dirs (filter (lambda (dir)
                                            (and (file-exists? dir)
                                                 (file-is-directory? dir)))
                                          elf-dirnames))
              (elf-pred (lambda (name stat)
                          (elf-file? name)))
              (elf-list (concatenate (map (lambda (dir)
                                            (find-files dir elf-pred))
                                          existing-elf-dirs))))
         (for-each strip-runpath elf-list)))))
  (for-each handle-output (alist-delete "debug" outputs))
  #t)

(define %standard-phases
  ;; The standard-phases of glib-or-gtk contains a superset of the phases
  ;; from the gnu-build-system.  If the glib-or-gtk? key is #f (the default)
  ;; then the extra phases will be removed again in (guix build-system meson).
  (modify-phases glib-or-gtk:%standard-phases
    (delete 'bootstrap)
    (replace 'configure configure)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)
    (add-after 'strip 'shrink-runpath shrink-runpath)))

(define* (meson-build #:key inputs phases
                      #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; meson-build-system.scm ends here
