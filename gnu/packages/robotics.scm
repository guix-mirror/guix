;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2022 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages robotics)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages xml))

(define-public enki
  ;; Previous versions use Qt4 and are unsuitable for Aseba.
  (let ((commit "afd2d8e2f91c095f6745505ca1f32f31ea874200")
        (revision "0"))
    (package
      (name "enki")
      (version (git-version "2.0pre" revision commit))
      (home-page "https://github.com/enki-community/enki/")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (sha256
                 (base32
                  "1d1901zzsfml97hb4mb3ah3ab1bk4kh7bn6m7xrj1rv0gk9wkhq7"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")))
      (native-inputs (list pkg-config))
      (inputs
       ;; Optionally, add Python + Boost for Python bindings.
       (list sdl2))
      (propagated-inputs
       ;; 'Viewer.h' includes 'QGLWidget'.
       (list qtbase-5 ;the viewer module needs Qt5 + MESA
             mesa))
      (synopsis "Robot simulator")
      (description
       "Enki is a robot simulator written in C++.  It provides collision and
limited physics support for robots evolving on a flat surface.  On a
contemporary desktop computer, Enki is able to simulate groups of robots
hundred times faster than real-time.")

      ;; The 'LICENSE' file as well as source file headers says that
      ;; researchers using the software are "asked" to cite using a given
      ;; citation, but that sentence is written as not being part of the
      ;; license (fortunately).
      (license license:gpl2+))))

(define-public aseba
  ;; Use the commit that allows us to build with Qt5.
  (let ((commit "3b35de80d5fdd47592b1c01d57e1f4ef37c5e5ea")
        (revision "0"))
    (package
      (name "aseba")
      (version (git-version "1.6.0" revision commit))
      (home-page "https://github.com/aseba-community/aseba")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)
                                    (recursive? #t))) ;for Blockly
                (sha256
                 (base32
                  "0j31lmc9f0ksvkh0md2fgsz92hcsrwnrqqcynamizs2ah8iwlqi5"))
                (file-name (string-append name "-" version "-checkout"))
                (modules '((guix build utils)))
                (snippet
                 ;; Add missing Qt5::Network.
                 '(substitute* "targets/playground/CMakeLists.txt"
                    (("target_link_libraries(.*)\\$\\{EXTRA_LIBS\\}" _ middle)
                     (string-append "target_link_libraries" middle
                                    " Qt5::Network ${EXTRA_LIBS}"))))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")
         #:parallel-build? #f                    ;occasionally fails with '-j'
         #:phases (modify-phases %standard-phases
                    (add-after 'unpack 'help-valgrind
                      (lambda* (#:key inputs #:allow-other-keys)
                        (let ((debug (search-input-directory inputs
                                                             "/lib/debug")))
                          (substitute* "tests/common/CMakeLists.txt"
                            (("--error-exitcode=1" flag)
                             (string-append "--extra-debuginfo-path="
                                            debug " " flag)))))))))
      (native-inputs
       (list pkg-config
             valgrind                             ;for tests
             `(,(canonical-package glibc) "debug")))
      (inputs
       (list dashel
             enki
             protobuf-3.5 ;for logging
             qtbase-5
             qtsvg
             qttools ;for libQt5Help, needed by "studio"
             qtwebkit
             qtx11extras
             eudev
             libxml2
             sdl2
             avahi))            ;XXX: we need the libdnssd compat layer
      (synopsis "Event-based robot programming tools")
      (description
       "Aseba means @dfn{actuator and sensor event-based architecture}.
It is a set of tools which allow beginners to program robots easily and
efficiently.  It includes robot simulators, a programming language, and a
visual programming language (VPL) that is notably used together with the
Thymio educational robot.")

      ;; Source file headers say "version 3.0" without "or any later version".
      (license license:lgpl3))))
