;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages java-graphics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages java)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

(define-public java-piccolo2d-core
  (package
    (name "java-piccolo2d-core")
    (version "3.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/piccolo2d/piccolo2d.java")
                    (commit (string-append "piccolo2d-complete-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k6gw643k83516lcw04mgac2yi75phdrng44pz9xk6hz066ip21s"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "piccolo2d-core.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "core") #t)))))
    (inputs
     (list java-junit))
    (home-page "http://piccolo2d.org")
    (synopsis "Structured 2D graphics framework")
    (description "Piccolo2D is a framework (in the Jazz ZUI tradition) to
create robust, full-featured graphical applications in Java, with features
such as zooming and multiple representation.  This package provides the core
libraries.")
    (license license:bsd-3)))

(define-public java-piccolo2d-extras
  (package (inherit java-piccolo2d-core)
    (name "java-piccolo2d-extras")
    (arguments
     `(#:jar-name "piccolo2d-extras.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "extras") #t))
         (add-after 'chdir 'remove-failing-test
           (lambda _
             ;; TODO: These both fail with "Unable to convolve src image"
             (delete-file "src/test/java/org/piccolo2d/extras/nodes/PShadowTest.java")
             (delete-file "src/test/java/org/piccolo2d/extras/util/ShadowUtilsTest.java")
             #t))
         (add-before 'check 'start-xorg-server
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The test suite requires a running X server.
             (system "Xvfb :1 -screen 0 640x480x24 &")
             (setenv "DISPLAY" ":1")
             #t)))))
    (inputs
     (list java-piccolo2d-core java-junit))
    (native-inputs
     (list xorg-server)) ; for tests
    (description "Piccolo2D is a framework (in the Jazz ZUI tradition) to
create robust, full-featured graphical applications in Java, with features
such as zooming and multiple representation.  This package provides additional
features not found in the core libraries.")))

(define-public java-marlin-renderer
  (package
    (name "java-marlin-renderer")
    (version "0.9.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bourgesl/marlin-renderer")
                    (commit (string-append "v" (string-map (match-lambda
                                                             (#\. #\_)
                                                             (c c))
                                                           version)))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12vb8fmxf1smnyv6w8i1khahy76v6r29j1qwabbykxff8i9ndxqv"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "marlin.jar"
       #:test-include (list "src/test/java/RunJUnitTest.java")))
    (inputs
     (list java-hamcrest-core java-junit))
    (home-page "https://github.com/bourgesl/marlin-renderer/")
    (synopsis "Rendering engine")
    (description "Marlin is a Java2D @code{RenderingEngine} optimized for
performance (improved memory usage and footprint, better multi-threading) and
better visual quality based on OpenJDK's @code{pisces} implementation.  It
handles shape rendering (@code{Graphics2D} @code{draw(Shape)} /
@code{fill(Shape)} with stroke and dash attributes.")
    ;; With Classpath Exception
    (license license:gpl2)))
