;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2021 Brendan Tildesley <mail@brendan.scot>
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
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx))

(define-public python-multivolumefile
  (package
    (name "python-multivolumefile")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "multivolumefile" version))
       (sha256
        (base32
         "0j46wab4b09s3favjzp3zs1cn2sn8pr7qyngs5wn31hpqqxbbz76"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pep517" ,python-pep517)
       ("python-setuptools" ,python-setuptools)
       ("python-setuptools-scm" ,python-setuptools-scm/next)
       ("python-coverage" ,python-coverage)
       ("python-coveralls" ,python-coveralls)
       ("python-pyannotate" ,python-pyannotate)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)))
    (home-page "https://github.com/miurahr/multivolume")
    (synopsis "Treat multiple files as one")
    (description "MultiVolumefile is a Python library that provides a
file-object abstraction, making it possible to use multiple files as if they
were a single file.")
    (license license:lgpl2.1+)))

(define-public python-bcj-cffi
  (package
    (name "python-bcj-cffi")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bcj-cffi" version))
       (sha256
        (base32
         "1jcczrb8zgg6w7v76w1wpz3nw75fghk3xwxkn09ll7kck7sdf68d"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-cffi" ,python-cffi)
       ("python-toml" ,python-toml)
       ("python-setuptools-scm" ,python-setuptools-scm/next)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-coverage" ,python-coverage)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)))
    (home-page "https://github.com/miurahr/bcj-cffi")
    (synopsis "Branch / Call /Jump CFFI library in Python")
    (description "This package provides an implementation of the Branch / Call /
Jump conversion filter by CFFI for Python.")
    (license license:lgpl2.1+)))

(define-public python-ppmd-cffi
  (package
    (name "python-ppmd-cffi")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ppmd-cffi" version))
       (sha256
        (base32
         "01wcd9l6pp6hivdmd275qh9dhcwficjqfl67hxix5n07vvq7jzz0"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-cffi" ,python-cffi)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-setuptools-scm" ,python-setuptools-scm/next)
       ("python-pep517" ,python-pep517)
       ("python-coverage" ,python-coverage)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)))
    (home-page "https://github.com/miurahr/ppmd")
    (synopsis "Prediction by Partial Matching compression library")
    (description "PPMd is a compression algorithm library using the Prediction
by Partial Matching statistical technique.  It is used in RAR and 7-Zip as one of
several possible methods.")
    (license license:lgpl2.1+)))

(define-public python-py7zr
  (package
    (name "python-py7zr")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "py7zr" version))
       (sha256
        (base32
         "1zmgp7yax328fj8yj8pj4l7yh78hp727j6wk12vfi6nmi82wl32i"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-bcj-cffi" ,python-bcj-cffi)
       ("python-multivolumefile" ,python-multivolumefile)
       ("python-ppmd-cffi" ,python-ppmd-cffi)
       ("python-pycryptodome" ,python-pycryptodome)
       ("python-pyzstd" ,python-pyzstd)
       ("python-texttable" ,python-texttable)
       ("python-zstandard" ,python-zstandard)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-setuptools-scm" ,python-setuptools-scm/next)
       ("python-coverage" ,python-coverage)
       ("python-coveralls" ,python-coveralls)
       ("python-libarchive-c" ,python-libarchive-c)
       ("python-py-cpuinfo" ,python-py-cpuinfo)
       ("python-pyannotate" ,python-pyannotate)
       ("python-pytest" ,python-pytest)
       ("python-pytest-benchmark" ,python-pytest-benchmark)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-remotedata" ,python-pytest-remotedata)
       ("python-pytest-timeout" ,python-pytest-timeout)))
    (home-page "https://github.com/miurahr/py7zr")
    (synopsis "7-zip in Python")
    (description "This package provides py7zr, which implements 7-zip
archive compression, decompression, encryption and decryption in
Python.")
    (license license:lgpl2.1+)))

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
         "0ghv1xbaq693kgww1x9c22bplz479ls9szjsaa4ig778ls834hm0"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove bundled copy of lz4.
           (delete-file-recursively "lz4libs")
           #t))))
    (build-system python-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-nose" ,python-nose)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (inputs
     `(("lz4" ,lz4)))
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

(define-public python-brotli
  (package
    (name "python-brotli")
    (version "1.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Brotli" version ".zip"))
        (sha256
         (base32
          "0f4433s4wv9masc303kkb1s0a09x81xwr8pdvj0kyxzdl05826sd"))))
    (build-system python-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://github.com/google/brotli")
    (synopsis "Python bindings for the Brotli compression library")
    (description
     "This package provides python bindings for the Brotli compression library.")
    (license license:asl2.0)))

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
  (package/inherit bitshuffle
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

(define-public python-zipp
  (package
    (name "python-zipp")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zipp" version))
       (sha256
        (base32
         "0v3qayhqv7vyzydpydwcp51bqciw8p2ajddw68x5k8zppc0vx3yk"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-more-itertools" ,python-more-itertools)))
    (native-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page "https://github.com/jaraco/zipp")
    (synopsis
     "Backport of pathlib-compatible object wrapper for zip files")
    (description
     "This package provides a @code{pathlib}-compatible @code{Zipfile} object
wrapper.  It provides a backport of the @code{Path} object.")
    (properties `((python2-variant . ,(delay python2-zipp))))
    (license license:expat)))

(define-public python2-zipp
  (let ((base (package-with-python2 (strip-python2-variant python-zipp))))
    (package/inherit
     base
     (native-inputs
      `(("python-contextlib2" ,python2-contextlib2)
        ("python-pathlib2" ,python2-pathlib2)
        ("python-unittest2" ,python2-unittest2)
        ,@(package-native-inputs base))))))

;; This package is used to bootstrap pytest, via importlib-metadata.
(define-public python2-zipp-bootstrap
  (hidden-package
   (package/inherit
    python2-zipp
    (name "python2-zipp-bootstrap")
    (arguments
     `(#:tests? #f
       ,@(package-arguments python2-zipp)))
    (native-inputs
     `(("python-setuptools-scm" ,python2-setuptools-scm))))))

(define-public python-zstandard
  (package
    (name "python-zstandard")
    (version "0.15.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zstandard" version))
       (sha256
        (base32 "0by9z7nxnkzhmza075q6q91rs8lnpf91129k8ppv7kymbwshipjj"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-hypothesis" ,python-hypothesis)))
    (home-page "https://github.com/indygreg/python-zstandard")
    (synopsis "Zstandard bindings for Python")
    (description "This project provides Python bindings for interfacing with
the Zstandard compression library.  A C extension and CFFI interface are
provided.")
    (license license:bsd-3)))

(define-public python-pyzstd
  (package
    (name "python-pyzstd")
    (version "0.14.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyzstd" version))
       (sha256
        (base32
         "1d3mngs45w2p490vrq5ymd2wz4lp15phmks1ilcx4k7amgibml3d"))))
    (build-system python-build-system)
    (home-page "https://github.com/animalize/pyzstd")
    (synopsis "Zstandard bindings for Python")
    (description "This package provides Python bindings to the Zstandard (zstd)
compression library.  The API is similar to Python's bz2/lzma/zlib module.")
    (license license:bsd-3)))
