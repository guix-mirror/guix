;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
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

(define-module (gnu packages storage)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages authentication)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public ceph
  (package
    (name "ceph")
    (version "14.2.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.ceph.com/tarballs/ceph-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0qiqhm6hvz299q54k3i4crnb5dhpq6xnn2yqih9pxn9van0dq1ln"))
              (patches
               (search-patches "ceph-boost-compat.patch"
                               "ceph-volume-respect-PATH.patch"
                               "ceph-disable-cpu-optimizations.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file-recursively
                            '(;; TODO: Unbundle these:
                              ;"src/isa-l"
                              ;"src/lua"
                              ;"src/xxHash"
                              ;"src/zstd"
                              ;"src/civetweb"
                              "src/seastar/fmt"
                              "src/test/downloads"
                              "src/c-ares"
                              "src/googletest"
                              "src/rapidjson"
                              "src/spdk"
                              "src/rocksdb"
                              "src/boost"))
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (let* ((out (assoc-ref %outputs "out"))
              (lib (assoc-ref %outputs "lib"))
              (libdir (string-append lib "/lib")))
         (list (string-append "-DCMAKE_INSTALL_PREFIX=" out)
               (string-append "-DCMAKE_INSTALL_LIBDIR=" libdir)
               (string-append "-DCMAKE_INSTALL_INCLUDEDIR="
                              lib "/include")
               ;; We need both libdir and libdir/ceph in RUNPATH.
               (string-append "-DCMAKE_INSTALL_RPATH="
                              libdir ";" libdir "/ceph")
               (string-append "-DCMAKE_INSTALL_SYSCONFDIR=" out "/etc")
               (string-append "-DCMAKE_INSTALL_DATADIR=" lib "/share")
               (string-append "-DCMAKE_INSTALL_MANDIR=" out "/share/man")
               (string-append "-DCMAKE_INSTALL_DOCDIR=" out "/share/ceph/doc")
               (string-append "-DCMAKE_INSTALL_LIBEXECDIR=" out "/libexec")
               (string-append "-DKEYUTILS_INCLUDE_DIR="
                              (assoc-ref %build-inputs "keyutils") "/include")
               (string-append "-DXFS_INCLUDE_DIR="
                              (assoc-ref %build-inputs "xfsprogs") "/include")
               "-DCMAKE_INSTALL_LOCALSTATEDIR=/var"
               "-DBUILD_SHARED_LIBS=ON"
               "-DWITH_SYSTEM_ROCKSDB=ON"
               "-DWITH_SYSTEM_BOOST=ON"
               "-DWITH_PYTHON3=ON"
               ;; TODO: Enable these when available in Guix.
               "-DWITH_MGR_DASHBOARD_FRONTEND=OFF"       ;requires node + nodeenv
               "-DWITH_BABELTRACE=OFF"
               "-DWITH_LTTNG=OFF"
               "-DWITH_SPDK=OFF"
               "-DWITH_RADOSGW_AMQP_ENDPOINT=OFF"

               ;; Use jemalloc instead of tcmalloc.
               "-DALLOCATOR=jemalloc"

               ;; Do not bother building the tests; we are not currently running
               ;; them, and they do not build with system googletest as of 14.2.5.
               "-DWITH_TESTS=OFF"))
       ;; FIXME: Some of the tests leak Btrfs subvolumes on Btrfs. See
       ;; <https://bugs.gnu.org/29674> for details. Disable tests until
       ;; resolved.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (lib (assoc-ref outputs "lib")))

               (substitute* "cmake/modules/Distutils.cmake"
                 ;; Prevent creation of Python eggs.
                 (("setup.py install")
                  "setup.py install --single-version-externally-managed --root=/"))

               (substitute* (find-files "src/pybind" "^setup\\.py$")
                 ;; Here we inject an extra line to the `setup.py' of the
                 ;; Python C libraries so RUNPATH gets set up correctly.
                 (("^([[:blank:]]+)extra_compile_args=(.*)$" _ indent args)
                  (string-append indent "extra_compile_args=" args
                                 indent "extra_link_args=['-Wl,-rpath="
                                 lib "/lib'],\n")))

               ;; Statically link libcrc32 because it does not get installed,
               ;; yet several libraries end up referring to it.
               (substitute* "src/common/CMakeLists.txt"
                 (("add_library\\(crc32")
                  "add_library(crc32 STATIC"))

               (substitute* "udev/50-rbd.rules"
                 (("/usr/bin/ceph-rbdnamer")
                  (string-append out "/bin/ceph-rbdnamer")))
               #t)))
         (add-before 'install 'set-install-environment
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (py3sitedir
                     (string-append out "/lib/python"
                                    ,(version-major+minor
                                      (package-version python))
                                    "/site-packages")))
               ;; The Python install scripts refuses to function if
               ;; the install directory is not on PYTHONPATH.
               (setenv "PYTHONPATH"
                       (string-append py3sitedir ":"
                                      (getenv "PYTHONPATH")))
               #t)))
         (add-after 'install 'wrap-python-scripts
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (scripts '("ceph" "ceph-mgr" "ceph-volume"))
                    (prettytable (assoc-ref inputs "python-prettytable"))
                    (six (assoc-ref inputs "python-six"))
                    (sitedir (lambda (package)
                               (string-append package
                                              "/lib/python"
                                              ,(version-major+minor
                                                (package-version python))
                                              "/site-packages")))
                    (PYTHONPATH (string-append
                                 (sitedir out) ":"
                                 (sitedir six) ":"
                                 (sitedir prettytable))))
               (for-each (lambda (executable)
                           (wrap-program (string-append out "/bin/" executable)
                             `("PYTHONPATH" ":" prefix (,PYTHONPATH))))
                         scripts)
               #t))))))
    (outputs
     '("out" "lib"))
    (native-inputs
     `(("gperf" ,gperf)
       ("pkg-config" ,pkg-config)
       ("python-cython" ,python-cython)
       ("python-sphinx" ,python-sphinx)
       ("yasm" ,yasm)))
    (inputs
     `(("boost" ,boost)
       ("curl" ,curl)
       ("cryptsetup" ,cryptsetup)
       ("expat" ,expat)
       ("fcgi" ,fcgi)
       ("fuse" ,fuse)
       ("jemalloc" ,jemalloc)
       ("keyutils" ,keyutils)
       ("leveldb" ,leveldb)
       ("libaio" ,libaio)
       ("libatomic-ops" ,libatomic-ops)
       ("libcap-ng" ,libcap-ng)
       ("libnl" ,libnl)
       ("lua" ,lua)
       ("lz4" ,lz4)
       ("oath-toolkit" ,oath-toolkit)
       ("openldap" ,openldap)
       ("openssl" ,openssl)
       ("ncurses" ,ncurses)
       ("nss" ,nss)
       ("python-prettytable" ,python-prettytable) ;used by ceph_daemon.py
       ("python-six" ,python-six)                 ;for ceph-mgr + plugins
       ("python" ,python-wrapper)
       ("rapidjson" ,rapidjson)
       ("rdma-core" ,rdma-core)
       ("rocksdb" ,rocksdb)
       ("snappy" ,snappy)
       ("udev" ,eudev)
       ("util-linux" ,util-linux)
       ("util-linux:lib" ,util-linux "lib")
       ("xfsprogs" ,xfsprogs)
       ("zlib" ,zlib)))
    (home-page "https://ceph.com/")
    (synopsis "Distributed object store and file system")
    (description
     "Ceph is a distributed storage system designed for reliability and
performance.  It provides network-based block devices (RBD), a POSIX
compliant file system (CephFS), and offers compatibility with various
storage protocols (S3, NFS, and others) through the RADOS gateway.")
    ;; The Ceph libraries are LGPL2.1 and most of the utilities fall under
    ;; GPL2. The installed erasure code plugins are BSD-3 licensed and do
    ;; not use the GPL code. The source archive includes a number of files
    ;; carrying other licenses; consult COPYING for more information. Note
    ;; that COPYING does not cover third-party bundled software.
    (license (list license:lgpl2.1 license:gpl2  ;some files are 'or later'
                   license:cc-by-sa3.0           ;documentation
                   license:bsd-3                 ;isa-l,jerasure,++
                   license:expat))))             ;civetweb,java bindings
