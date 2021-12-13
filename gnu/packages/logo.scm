;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu packages logo)
  #:use-module (gnu packages qt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu))

(define-public qlogo
  (package
    (name "qlogo")
    (version "0.92")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://qlogo.org/assets/sources/QLogo-"
                           version ".tgz"))
       (sha256
        (base32
         "0cpyj1ji6hjy7zzz05672f0j6fr0mwpc1y3sq36hhkv2fkpidw22"))))
    (build-system gnu-build-system)
    (inputs
     (list qtbase-5))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "QLogo.pro"
               (("target\\.path = /usr/bin")
                (string-append "target.path = "
                               (assoc-ref outputs "out") "/bin")))
             (invoke "qmake" "QLogo.pro")))
         ;; The check phase rebuilds the source for tests. So, it needs to be
         ;; run after the install phase has installed the outputs of the build
         ;; phase.
         (delete 'check)
         (add-after 'install 'check
           (lambda _
             ;; Clean files created by the build phase.
             (invoke "make" "clean")
             ;; QLogo tries to create its "dribble file" in the home
             ;; directory. So, set HOME.
             (setenv "HOME" "/tmp")
             ;; Build and run tests.
             (invoke "qmake" "TestQLogo.pro")
             (invoke "make" "-j" (number->string (parallel-job-count)))
             (invoke "./testqlogo"))))))
    (home-page "https://qlogo.org")
    (synopsis "Logo interpreter using Qt and OpenGL")
    (description "QLogo is an interpreter for the Logo language written in C++
using Qt and OpenGL.  Specifically, it mimics, as reasonably as possible, the
UCBLogo interpreter.")
    (license license:gpl2+)))
