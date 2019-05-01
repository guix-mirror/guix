;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (guix build linux-module-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            linux-module-build))

;; Commentary:
;;
;; Builder-side code of linux-module build.
;;
;; Code:

;; Copied from make-linux-libre's "configure" phase.
(define* (configure #:key inputs target #:allow-other-keys)
  (setenv "KCONFIG_NOTIMESTAMP" "1")
  (setenv "KBUILD_BUILD_TIMESTAMP" (getenv "SOURCE_DATE_EPOCH"))
  ;(let ((arch ,(system->linux-architecture
  ;                         (or (%current-target-system)
  ;                             (%current-system)))))
  ;  (setenv "ARCH" arch)
  ;  (format #t "`ARCH' set to `~a'~%" (getenv "ARCH")))
  (when target
    (setenv "CROSS_COMPILE" (string-append target "-"))
    (format #t "`CROSS_COMPILE' set to `~a'~%"
               (getenv "CROSS_COMPILE")))
  ; TODO: (setenv "EXTRA_VERSION" ,extra-version)
  ; TODO: kernel ".config".
  #t)

(define* (build #:key inputs make-flags #:allow-other-keys)
  (apply invoke "make" "-C"
         (string-append (assoc-ref inputs "linux-module-builder")
                        "/lib/modules/build")
         (string-append "M=" (getcwd))
         (or make-flags '())))

;; This block was copied from make-linux-libre--only took the "modules_install"
;; part.
(define* (install #:key inputs native-inputs outputs #:allow-other-keys)
  (let* ((out (assoc-ref outputs "out"))
         (moddir (string-append out "/lib/modules"))
         (kmod (assoc-ref (or native-inputs inputs) "kmod")))
    ;; Install kernel modules
    (mkdir-p moddir)
    (invoke "make" "-C"
            (string-append (assoc-ref inputs "linux-module-builder")
                           "/lib/modules/build")
            (string-append "M=" (getcwd))
            (string-append "DEPMOD=" kmod "/bin/depmod")
            (string-append "MODULE_DIR=" moddir)
            (string-append "INSTALL_PATH=" out)
            (string-append "INSTALL_MOD_PATH=" out)
            "INSTALL_MOD_STRIP=1"
            "modules_install")))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (replace 'configure configure)
    (replace 'build build)
    (replace 'install install)))

(define* (linux-module-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order, with a Linux kernel in attendance."
  (apply gnu:gnu-build
         #:inputs inputs #:phases phases
         args))

;;; linux-module-build-system.scm ends here
