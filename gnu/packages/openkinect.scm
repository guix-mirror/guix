;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages openkinect)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages image-processing))

(define-public libfreenect
  (let ((version "0.6.2"))
    (package
      (name "libfreenect")
      (version version)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/OpenKinect/libfreenect")
                      (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "02pb8mcl62kzdcgbnv3rw4nl0f08iw8pjiqqhfy3ycpkvyppw97w"))))
      (build-system cmake-build-system)
      (arguments
       '(#:tests? #f                    ; package has no tests
         #:configure-flags
         '("-DBUILD_FAKENECT=ON"
           "-DBUILD_CPP=ON"
           "-DBUILD_EXAMPLES=OFF"       ; available in libfreenect-examples
           "-DBUILD_CV=OFF"             ; available in libfreenect-cv
           "-DBUILD_PYTHON=OFF"         ; available in python-libfreenect
           "-DBUILD_C_SYNC=ON")
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'install-udev-rules
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (rules-out (string-append out "/lib/udev/rules.d")))
                 (install-file "../source/platform/linux/udev/51-kinect.rules"
                               (string-append rules-out "51-kinect.rules"))
                 #t))))))
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (inputs
       `(("libusb" ,libusb)))
      (synopsis "Drivers and libraries for the Xbox Kinect device")
      (description "libfreenect is a userspace driver for the Microsoft Kinect.
It supports: RGB and Depth Images, Motors, Accelerometer, LED and Audio.")
      (home-page "https://openkinect.org/")
      (license license:gpl2+))))

;; Library are already compiled in libfreenect, avoid build it again.
(define libfreenect-derived-phases
  '(modify-phases %standard-phases
     (add-after 'unpack 'patch-CMakeLists.txt
       (lambda* (#:key outputs #:allow-other-keys)
         (substitute* "CMakeLists.txt"
           ((".*libusb.*") "")
           (("add_subdirectory \\(src\\)") "")
           ((".*libfreenectConfig.cmake.*") ""))
         #t))))

(define-public libfreenect-examples
  (package
    (inherit libfreenect)
    (name "libfreenect-examples")
    (inputs
     `(("libfreenect" ,libfreenect)
       ("glut" ,freeglut)))
    (arguments
     `(#:tests? #f                      ; package has no tests
       #:configure-flags '("-DBUILD_EXAMPLES=ON"
                           "-DBUILD_FAKENECT=OFF"
                           "-DBUILD_CPP=OFF"
                           "-DBUILD_C_SYNC=OFF"
                           "-DBUILD_CV=OFF")
       #:phases ,libfreenect-derived-phases))
    (synopsis "Examples for libfreenect, the Xbox Kinect device library")))

(define-public libfreenect-opencv
  (package
    (inherit libfreenect)
    (name "libfreenect-opencv")
    (inputs
     `(("libfreenect" ,libfreenect)
       ("opencv" ,opencv)))
    (arguments
     `(#:tests? #f                      ; package has no tests
       #:configure-flags '("-DBUILD_EXAMPLES=OFF"
                           "-DBUILD_FAKENECT=OFF"
                           "-DBUILD_CPP=OFF"
                           "-DBUILD_C_SYNC=OFF"
                           "-DBUILD_CV=ON")
       #:phases ,libfreenect-derived-phases))
    (synopsis "OpenCV wrapper for libfreenect, the Xbox Kinect device
library")))

(define-public python-libfreenect
  (package
    (inherit libfreenect)
    (name "python-libfreenect")
    (native-inputs
     `(("python-cython" ,python-cython)))
    (inputs
     `(("libfreenect" ,libfreenect)))
    (propagated-inputs
     `(("python" ,python)
       ("python-numpy" ,python-numpy)))
    (arguments
     `(#:tests? #f                      ; package has no tests
       #:configure-flags '("-DBUILD_EXAMPLES=OFF"
                           "-DBUILD_FAKENECT=OFF"
                           "-DBUILD_CPP=OFF"
                           "-DBUILD_C_SYNC=OFF"
                           "-DBUILD_CV=OFF"
                           "-DBUILD_PYTHON3=ON")
       #:phases ,libfreenect-derived-phases))
    (synopsis "Python wrapper for libfreenect, the Xbox Kinect device
library")))
