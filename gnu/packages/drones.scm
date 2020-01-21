;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu packages drones)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xml)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

(define (ardupilot-type->tag type)
  (case type
    ((copter) "Copter")
    ((plane)  "ArduPlane")
    ((rover)  "Rover")
    (else #f)))

(define (ardupilot-type->waf-cmd type)
  (symbol->string type))

(define* (make-ardupilot-firmware #:key name version base32 type board target)
  (package
    (name (string-append name "-" board))
    (version version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ArduPilot/ardupilot")
             (commit (string-append
                      (ardupilot-type->tag type) "-" version))
             ;; XXX: Ardupilot includes several git submodules. They should be
             ;; avoided but as this is not supported upstream, and not trivial
             ;; to fix, keep it this way for now.
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256 base32)))

    ;; Could also be waf-build-system but every phase has to be rewritten
    ;; anyway.
    (build-system gnu-build-system)
    (arguments
     `(#:imported-modules ((gnu build cross-toolchain)
                           ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)

         ;; Remove the root waf script that relies on waf git submodule.
         (add-before 'configure 'setup-waf
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             (let ((waf (assoc-ref (or native-inputs inputs) "waf")))
               (delete-file "waf")
               (copy-file (string-append waf "/bin/waf") "waf"))
             #t))

         ;; When cross-compiling, we do not want to use the default gnu
         ;; cross-compiler, so set CROSS_CPATH and CROSS_LIBRARY_PATH
         ;; variables ourselves instead.
         (delete 'set-cross-path)
         (add-before 'configure 'set-custom-cross-cpath
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             ((@@ (gnu build cross-toolchain) set-cross-path)
              #:inputs
              `(("libc" . ,(assoc-ref (or native-inputs inputs)
                                      "ardupilot-cross-libc"))
                ("xkernel-headers" .
                 ,(assoc-ref (or native-inputs inputs)
                             "ardupilot-cross-kernel-headers"))))
             ;; We need to produce a static binary, so that it can works on
             ;; other systems than Guix System. Add a static version of the
             ;; cross libc to CROSS_LIBRARY_PATH variable.
             (setenv "CROSS_LIBRARY_PATH"
                     (string-append
                      (getenv "CROSS_LIBRARY_PATH") ":"
                      (assoc-ref (or native-inputs inputs)
                                 "ardupilot-cross-libc-static") "/lib"))
             #t))

         ;; Remove dependencies to 'git'.
         (add-before 'configure 'remove-git
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "wscript"
               (("^.*cfg\\.load\\('git_submodule.*$")
                ""))
             (substitute* "Tools/ardupilotwaf/boards.py"
               (("^.*GIT_VERSION.*$")
                ""))
             #t))

         ;; Configure for the given BOARD, and force a static build for
         ;; reasons exposed above.
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "./waf" "configure" "--board" ,board "--static")
             #t))

         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "./waf" ,(ardupilot-type->waf-cmd type))
             #t))

         ;; Do not run tests as we are always cross-compiling.
         (delete 'check)

         ;; Install the produced firmware.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (copy-recursively
                (string-append "build/" ,board "/bin") bin))
             #t)))))
    (native-inputs
     `(("waf" ,python-waf)
       ("python" ,python)
       ("python-future" ,python-future)
       ("python-lxml" ,python-lxml)

       ;; Packages needed for cross-compiling the firmware.
       ("ardupilot-cross-gcc" ,(cross-gcc target
                                          #:xbinutils
                                          (cross-binutils target)
                                          #:libc
                                          (cross-libc target)))
       ("ardupilot-cross-libc" ,(cross-libc target))
       ("ardupilot-cross-libc-static" ,(cross-libc target) "static")
       ("ardupilot-cross-kernel-headers"
        ,@(assoc-ref (package-propagated-inputs
                      (cross-libc target))
                     "kernel-headers"))
       ("ardupilot-cross-binutils" ,(cross-binutils target))
       ("ardupilot-cross-pkg-config" ,(parameterize ((%current-target-system
                                                      target))
                                        pkg-config))))
    (home-page "https://ardupilot.org/")
    (synopsis "Unmanned vehicle autopilot software suite")
    (description "@code{ardupilot} is an unmanned vehicle autopilot software
suite capable of controlling autonomous:
@itemize
@item multirotor drones
@item fixed-wing and vtol aircraft
@item helicopters
@item ground rovers
@item boats
@item submarines
@item antenna trackers
@end itemize")
    (license gpl3+)))

(define (make-arducopter-firmware board target)
  (make-ardupilot-firmware
   #:name "arducopter"
   #:version "3.6.11"
   #:base32 (base32 "1zkr2nhkksmrriirs2dnp8a0gcf9rfqw1x86pzhh6w4ciqwpidqn")
   #:type 'copter
   #:board board
   #:target target))

(define (make-arduplane-firmware board target)
  (make-ardupilot-firmware
   #:name "arduplane"
   #:version "4.0.1"
   #:base32 (base32 "0awafvrppg4ilwpbhw88r5xkbgqrmqypsn6lbzyi6bz0zy5cfhb5")
   #:type 'plane
   #:board board
   #:target target))

(define-public arducopter-bbbmini
  (make-arducopter-firmware "bbbmini" "arm-linux-gnueabihf"))

(define-public arduplane-bbbmini
  (make-arduplane-firmware "bbbmini" "arm-linux-gnueabihf"))

;; Firmware for Bebop and Bebop2 drones.
(define-public arducopter-bebop
  (make-arducopter-firmware "bebop" "arm-linux-gnueabihf"))
