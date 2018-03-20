;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
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
  #:use-module (guix build rpath)
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
                 ,@configure-flags
                 ,source-dir)))

    ;; Meson lacks good facilities for dealing with RUNPATH, so we
    ;; add the output "lib" directory here to avoid doing that in
    ;; many users.  Related issues:
    ;; * <https://github.com/mesonbuild/meson/issues/314>
    ;; * <https://github.com/mesonbuild/meson/issues/3038>
    ;; * <https://github.com/NixOS/nixpkgs/issues/31222>
    (setenv "LDFLAGS" (string-append "-Wl,-rpath=" out "/lib"))

    (mkdir build-dir)
    (chdir build-dir)
    (zero? (apply system* "meson" args))))

(define* (build #:key parallel-build?
                #:allow-other-keys)
  "Build a given meson package."
  (zero? (apply system* "ninja"
                (if parallel-build?
                    `("-j" ,(number->string (parallel-job-count)))
                    '("-j" "1")))))

(define* (check #:key test-target parallel-tests? tests?
                #:allow-other-keys)
  (setenv "MESON_TESTTHREADS"
          (if parallel-tests?
              (number->string (parallel-job-count))
              "1"))
  (if tests?
      (zero? (system* "ninja" test-target))
      (begin
        (format #t "test suite not run~%")
        #t)))

(define* (install #:rest args)
  (zero? (system* "ninja" "install")))

(define* (fix-runpath #:key (elf-directories '("lib" "lib64" "libexec"
                                               "bin" "sbin"))
                      outputs #:allow-other-keys)
  "Try to make sure all ELF files in ELF-DIRECTORIES are able to find their
local dependencies in their RUNPATH, by searching for the needed libraries in
the directories of the package, and adding them to the RUNPATH if needed.
Also shrink the RUNPATH to what is needed,
since a lot of directories are left over from the build phase of meson,
for example libraries only needed for the tests."

  ;; Find the directories (if any) that contains DEP-NAME.  The directories
  ;; searched are the ones that ELF-FILES are in.
  (define (find-deps dep-name elf-files)
    (map dirname (filter (lambda (file)
                           (string=? dep-name (basename file)))
                         elf-files)))

  ;; Return a list of libraries that FILE needs.
  (define (file-needed file)
    (let* ((elf (call-with-input-file file
                  (compose parse-elf get-bytevector-all)))
           (dyninfo (elf-dynamic-info elf)))
      (if dyninfo
          (elf-dynamic-info-needed dyninfo)
          '())))


  ;; If FILE needs any libs that are part of ELF-FILES, the RUNPATH
  ;; is modified accordingly.
  (define (handle-file file elf-files)
    (let* ((dep-dirs (concatenate (map (lambda (dep-name)
                                         (find-deps dep-name elf-files))
                                       (file-needed file)))))
      (unless (null? dep-dirs)
        (augment-rpath file (string-join dep-dirs ":")))))

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
         (for-each (lambda (elf-file)
                     (system* "patchelf" "--shrink-rpath" elf-file)
                     (handle-file elf-file elf-list))
                   elf-list)))))
  (for-each handle-output outputs)
  #t)

(define %standard-phases
  ;; The standard-phases of glib-or-gtk contains a superset of the phases
  ;; from the gnu-build-system.  If the glib-or-gtk? key is #f (the default)
  ;; then the extra phases will be removed again in (guix build-system meson).
  (modify-phases glib-or-gtk:%standard-phases
    (replace 'configure configure)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)
    (add-after 'strip 'fix-runpath fix-runpath)))

(define* (meson-build #:key inputs phases
                      #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; meson-build-system.scm ends here
