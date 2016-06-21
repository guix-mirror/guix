;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
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

(define-module (gnu packages serialization)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages pkg-config))

(define-public cereal
  (package
    (name "cereal")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/USCiLab/cereal/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13bvsfzvm7yyp97k20iznq2j14dj3qiczvpq7g8897njw46psq25"))))
    (build-system cmake-build-system)
    (arguments
     `(;; The only included tests are portability tests requiring
       ;; cross-compilation and boost.  Since we are building cereal on more
       ;; platforms anyway, there is no compelling reason to build the tests.
       #:tests? #f
       #:out-of-source? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
          (lambda _
            (substitute* "doc/doxygen.in"
              (("@CMAKE_CURRENT_SOURCE_DIR@") "."))
            (zero? (system* "doxygen" "doc/doxygen.in"))))
         ;; There is no "install" target, so we have to provide our own
         ;; "install" phase.
         (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out     (assoc-ref outputs "out"))
                   (doc     (string-append out "/share/cereal/docs"))
                   (include (string-append out "/include/cereal")))
              (mkdir-p doc)
              (mkdir-p include)
              (copy-recursively "include/cereal" include)
              (copy-recursively "doc/html" doc))
            #t)))))
    (native-inputs
     `(("doxygen" ,doxygen)))
    (home-page "http://uscilab.github.io/cereal/")
    (synopsis "C++11 library for serialization")
    (description
     "Cereal is a header-only C++11 serialization library.  Cereal takes
arbitrary data types and reversibly turns them into different representations,
such as compact binary encodings, XML, or JSON.")
    (license license:bsd-3)))


(define-public msgpack
  (package
    (name "msgpack")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/msgpack/msgpack-c/releases/download/"
         "cpp-" version "/msgpack-" version ".tar.gz"))
       (snippet
        '(let ((p (open-file "msgpack.pc.in" "a")))
           (begin
             (display
              (string-append "Requires: " "zlib" "\n") p)
             (close-output-port p))))
       (sha256
        (base32
         "0bpjfh9vz0n2k93mph3x15clmigkgs223xfn8h12ymrh5gsi5ica"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("googletest" ,googletest)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("zlib" ,zlib))) ;; Msgpack installs two headers (zbuffer.h,
    ;; zbuffer.hpp) which #include <zlib.h>.  However, 'guix gc --references'
    ;; does not detect a store reference to zlib since these headers are not
    ;; compiled.
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autoconf
           (lambda _
             (system* "autoreconf" "-vfi"))))))
    (home-page "http://www.msgpack.org")
    (synopsis "Binary serialization library")
    (description "Msgpack is a library for C/C++ that implements binary
serialization.")
    (license license:boost1.0)))
