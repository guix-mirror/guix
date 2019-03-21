;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages python-compression)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define-public python-lzo
  (package
    (name "python-lzo")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-lzo" version))
       (sha256
        (base32
         "0iakqgd51n1cd7r3lpdylm2rgbmd16y74cra9kcapwg84mlf9a4p"))))
    (build-system python-build-system)
    (arguments
     `(#:test-target "check"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-setuppy
           (lambda _
             (substitute* "setup.py"
               (("include_dirs.append\\(.*\\)")
                (string-append "include_dirs.append('"
                               (assoc-ref %build-inputs "lzo")
                               "/include/lzo"
                               "')")))
             #t)))))
    (inputs
     `(("lzo" ,lzo)))
    (home-page "https://github.com/jd-boyd/python-lzo")
    (synopsis "Python bindings for the LZO data compression library")
    (description
     "Python-LZO provides Python bindings for LZO, i.e. you can access
the LZO library from your Python scripts thereby compressing ordinary
Python strings.")
    (license license:gpl2+)))

(define-public python2-lzo
  (package-with-python2 python-lzo))

(define-public python-lz4
  (package
    (name "python-lz4")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lz4" version))
       (sha256
        (base32
         "0ghv1xbaq693kgww1x9c22bplz479ls9szjsaa4ig778ls834hm0"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page "https://github.com/python-lz4/python-lz4")
    (synopsis "LZ4 bindings for Python")
    (description
     "This package provides python bindings for the lz4 compression library
by Yann Collet.  The project contains bindings for the LZ4 block format and
the LZ4 frame format.")
    (license license:bsd-3)))

(define-public python2-lz4
  (package-with-python2 python-lz4))

(define-public python-lzstring
  (package
    (name "python-lzstring")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lzstring" version))
       (sha256
        (base32
         "18ly9pppy2yspxzw7k1b23wk77k7m44rz2g0271bqgqrk3jn3yhs"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-future" ,python-future)))
    (home-page "https://github.com/gkovacs/lz-string-python")
    (synopsis "String compression")
    (description "Lz-string is a string compressor library for Python.")
    (license license:expat)))

(define-public python2-lzstring
  (package-with-python2 python-lzstring))

(define-public bitshuffle
  (package
    (name "bitshuffle")
    (version "0.3.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "bitshuffle" version))
              (sha256
               (base32
                "1823x61kyax4dc2hjmc1xraskxi1193y8lvxd03vqv029jrj8cjy"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove generated Cython files.
                  (delete-file "bitshuffle/h5.c")
                  (delete-file "bitshuffle/ext.c")
                  #t))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f             ; fail: https://github.com/h5py/h5py/issues/769
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-neon-detection
           ;; Neon is only for aarch64 ATM
           ;; see: https://github.com/kiyo-masui/bitshuffle/pull/73
           (lambda _
             (substitute* "src/bitshuffle_core.c"
               (("#define USEARMNEON")
                "#ifdef __aarch64__\n#define USEARMNEON\n#endif"))
             #t))
         (add-after 'unpack 'dont-build-native
           (lambda _
             (substitute* "setup.py"
               (("'-march=native', ") ""))
             #t)))))
    (inputs
     `(("numpy" ,python-numpy)
       ("h5py" ,python-h5py)
       ("hdf5" ,hdf5)))
    (native-inputs
     `(("cython" ,python-cython)))
    (home-page "https://github.com/kiyo-masui/bitshuffle")
    (synopsis "Filter for improving compression of typed binary data")
    (description "Bitshuffle is an algorithm that rearranges typed, binary data
for improving compression, as well as a python/C package that implements this
algorithm within the Numpy framework.")
    (license license:expat)))

(define-public bitshuffle-for-snappy
  (package
    (inherit bitshuffle)
    (name "bitshuffle-for-snappy")
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments bitshuffle)
       ((#:tests? _ #f) #f)
       ((#:phases phases)
        `(modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (with-output-to-file "Makefile"
                 (lambda _
                   (format #t "\
libbitshuffle.so: src/bitshuffle.o src/bitshuffle_core.o src/iochain.o lz4/lz4.o
\tgcc -O3 -ffast-math -std=c99 -o $@ -shared -fPIC $^

%.o: %.c
\tgcc -O3 -ffast-math -std=c99 -fPIC -Isrc -Ilz4 -c $< -o $@

PREFIX:=~a
LIBDIR:=$(PREFIX)/lib
INCLUDEDIR:=$(PREFIX)/include

install: libbitshuffle.so
\tinstall -dm755 $(LIBDIR)
\tinstall -dm755 $(INCLUDEDIR)
\tinstall -m755 libbitshuffle.so $(LIBDIR)
\tinstall -m644 src/bitshuffle.h $(INCLUDEDIR)
\tinstall -m644 src/bitshuffle_core.h $(INCLUDEDIR)
\tinstall -m644 src/iochain.h $(INCLUDEDIR)
\tinstall -m644 lz4/lz4.h $(INCLUDEDIR)
" (assoc-ref outputs "out"))))
               #t))))))
    (inputs '())
    (native-inputs '())))
