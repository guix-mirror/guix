;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Marius Bakke <mbakke@fastmail.com>
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
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public ceph
  (package
    (name "ceph")
    (version "13.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.ceph.com/tarballs/ceph-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0jbs6l763clbqnq2s5jksn44yf71rhcnk85cw64fqvmv0r4ch71n"))
              (patches
               (search-patches "ceph-skip-unittest_blockdev.patch"
                               "ceph-skip-collect-sys-info-test.patch"
                               "ceph-detect-rocksdb.patch"
                               "ceph-volume-respect-PATH.patch"
                               "ceph-disable-cpu-optimizations.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file-recursively
                            '(;; TODO: Unbundle these:
                              ;"src/isa-l"
                              ;"src/lua"
                              ;"src/googletest"
                              ;"src/xxHash"
                              ;"src/zstd"
                              ;"src/civetweb"
                              "src/test/downloads"
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
               ;; We need both libdir and libdir/ceph in RUNPATH.
               (string-append "-DCMAKE_INSTALL_RPATH="
                              libdir ";" libdir "/ceph")
               (string-append "-DCMAKE_INSTALL_SYSCONFDIR=" out "/etc")
               (string-append "-DCMAKE_INSTALL_MANDIR=" out "/share/man")
               (string-append "-DCMAKE_INSTALL_DOCDIR=" out "/share/ceph/doc")
               (string-append "-DCMAKE_INSTALL_LIBEXECDIR=" out "/libexec")
               (string-append "-DKEYUTILS_INCLUDE_DIR="
                              (assoc-ref %build-inputs "keyutils") "/include")
               "-DCMAKE_INSTALL_LOCALSTATEDIR=/var"
               "-DENABLE_SHARED=ON"
               "-DWITH_SYSTEM_ROCKSDB=ON"
               "-DWITH_SYSTEM_BOOST=ON"
               "-DWITH_PYTHON3=ON"
               ;; TODO: Enable these when available in Guix.
               "-DWITH_MGR_DASHBOARD_FRONTEND=OFF"       ;requires node + nodeenv
               "-DWITH_BABELTRACE=OFF"
               "-DWITH_LTTNG=OFF"
               "-DWITH_SPDK=OFF"
               "-DWITH_XFS=OFF"
               "-DWITH_XIO=OFF"
               ;; Use jemalloc instead of tcmalloc.
               "-DALLOCATOR=jemalloc"))
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

               ;; Make header files follow the dynamic libraries.
               (substitute* "src/include/CMakeLists.txt"
                 (("DESTINATION include")
                  (string-append "DESTINATION " lib "/include")))

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

               (substitute* "src/ceph-disk/tox.ini"
                 ;; Disable flake8 test since it complains about too long lines.
                 (("envlist = flake8,py27") "envlist = py27"))

               (substitute* "src/ceph-detect-init/tox.ini"
                 ;; Disable python3 tests until we at least get py2 working.
                 (("envlist = pep8,py27,py3") "envlist = pep8,py27"))

               (substitute* "src/key_value_store/kv_flat_btree_async.cc"
                 (("/usr/include/") ""))

               (substitute* "src/test/test_subprocess.cc"
                 (("/bin/sh") (which "sh")))
               (substitute* "qa/standalone/special/ceph_objectstore_tool.py"
                 (("/bin/rm") (which "rm")))
               (substitute* "src/ceph-disk/ceph_disk/main.py"
                 (("/bin/mount") "mount")
                 (("/bin/umount") "umount")
                 (("/sbin/blkid") (which "blkid"))
                 (("'cryptsetup'") (string-append "'" (which "cryptsetup") "'"))
                 (("'sgdisk'") (string-append "'" (which "sgdisk") "'"))
                 (("'parted'") (string-append "'" (which "parted") "'"))
                 (("'udevadm'") (string-append "'" (which "udevadm") "'")))

               (substitute* "udev/50-rbd.rules"
                 (("/usr/bin/ceph-rbdnamer")
                  (string-append out "/bin/ceph-rbdnamer")))
               (substitute* "udev/60-ceph-by-parttypeuuid.rules"
                 (("/sbin/blkid") (which "blkid")))
               (substitute* "udev/95-ceph-osd.rules"
                 (("/usr/sbin/ceph-disk")
                  (string-append out "/bin/ceph-disk")))

               (substitute* "src/test/run-cli-tests"
                 ;; Use our python-cram instead of the (un)bundled one.
                 (("CRAM_BIN=.*$")
                  (string-append "CRAM_BIN=" (which "cram") "\n")))

               ;; Disable tests that are known to fail.
               ;; TODO: The majority of these fail because
               ;; 'qa/workunits/ceph-helpers.sh' expects to find
               ;; /tmp/ceph-disk-virtualenv/bin/ceph-disk, but somehow
               ;; src/ceph-disk/CMakeLists.txt fails to create it.
               (substitute* "src/test/CMakeLists.txt"
                 ;; FIXME: These tests fails because `ceph-disk'
                 ;; is not available.
                 (("^add_ceph_test\\(test-ceph-helpers\\.sh.*$") "\n")
                 (("^add_ceph_test\\(test_pidfile\\.sh.*$") "\n")
                 ;; XXX Why does this fail.
                 (("^add_ceph_test\\(cephtool-test-mon\\.sh.*$") "\n")
                 ;; This fails due to missing '/etc/fstab'.
                 (("^add_ceph_test\\(cephtool-test-rados\\.sh.*$") "\n")
                 ;; `Bad messages to stderr: OSD has the store locked'
                 (("^add_ceph_test\\(ceph_objectstore_tool\\.py.*$") "\n")
                 ;; The bundled python-cram fork needs patching to work on
                 ;; guix, and the system version does not support --error-dir.
                 ;; https://bitbucket.org/brodie/cram/issues/9
                 (("^add_ceph_test\\(run-cli-tests.*$") "\n")
                 ;; FIXME: tox/virtualenv/pip does not discover the
                 ;; required packages and tries to go online.
                 (("^add_test\\(NAME run-tox-ceph-disk.*$") "\n")
                 (("^add_test\\(NAME run-tox-ceph-detect-init.*$") "\n")
                 ;; Also remove from the set_property block.
                 (("run-tox-ceph-disk") "")
                 (("run-tox-ceph-detect-init") ""))
               ;; TODO: This also seems to fail because of /etc/os-release.
               ;; How to make src/common/util.cc behave without it.
               (substitute* "src/test/crush/CMakeLists.txt"
                 (("^add_ceph_test\\(crush-classes\\.sh.*$") "\n"))
               ;; More 'ceph-disk' issues here.. :-(
               (substitute* "src/test/erasure-code/CMakeLists.txt"
                 (("^add_ceph_test\\(test-erasure-code-plugins\\.sh.*$") "\n")
                 (("^add_ceph_test\\(test-erasure-code\\.sh.*$") "\n")
                 (("^add_ceph_test\\(test-erasure-eio\\.sh.*$") "\n"))
               (substitute* "src/test/libradosstriper/CMakeLists.txt"
                 (("^add_ceph_test\\(rados-striper\\.sh.*$") "\n"))
               (substitute* "src/test/mon/CMakeLists.txt"
                 (("^add_ceph_test\\(osd-crush\\.sh.*$") "\n")
                 (("^add_ceph_test\\(test_pool_quota\\.sh.*$") "\n")
                 (("^add_ceph_test\\(osd-pool-create\\.sh.*$") "\n"))
               (substitute* "src/test/osd/CMakeLists.txt"
                 (("^add_ceph_test\\(osd-bench\\.sh.*$") "\n")
                 (("^add_ceph_test\\(osd-config\\.sh.*$") "\n")
                 (("add_ceph_test\\(osd-dup\\.sh.*$") "\n")
                 (("^add_ceph_test\\(osd-markdown\\.sh.*$") "\n")
                 (("^add_ceph_test\\(osd-reactivate\\.sh.*$") "\n")
                 (("^add_ceph_test\\(osd-reuse-id\\.sh.*$") "\n")
                 (("^add_ceph_test\\(osd-scrub-repair\\.sh.*$") "\n")
                 (("^add_ceph_test\\(osd-scrub-snaps\\.sh.*$") "\n")
                 (("^add_ceph_test\\(osd-copy-from\\.sh.*$") "\n")
                 (("^add_ceph_test\\(osd-fast-mark-down\\.sh.*$") "\n"))
               #t)))
         (add-before 'configure 'gcc-workaround
           (lambda _
             (unsetenv "C_INCLUDE_PATH")
             (unsetenv "CPLUS_INCLUDE_PATH")
             #t))
         (add-before 'check 'set-check-environment
           (lambda _
             ;; Run tests in parallel.
             (setenv "CTEST_PARALLEL_LEVEL"
                     (number->string (parallel-job-count)))
             ;; `pip' requires write access in $HOME.
             (setenv "HOME" "/tmp")
             #t))
         (add-before 'install 'set-install-environment
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (py2sitedir
                     (string-append out "/lib/python2.7/site-packages"))
                    (py3sitedir
                     (string-append out "/lib/python"
                                    ,(version-major+minor
                                      (package-version python))
                                    "/site-packages")))
               ;; The Python install scripts refuses to function if
               ;; the install directory is not on PYTHONPATH.
               (setenv "PYTHONPATH"
                       (string-append py2sitedir ":" py3sitedir ":"
                                      (getenv "PYTHONPATH")))
               #t)))
         (add-after 'install 'wrap-python-scripts
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (scripts '("ceph" "ceph-mgr" "ceph-volume"
                               "ceph-detect-init"
                               "ceph-disk")) ;deprecated
                    (prettytable (assoc-ref inputs "python2-prettytable"))
                    (six (assoc-ref inputs "python2-six"))
                    (sitedir (lambda (package)
                               (string-append package
                                              "/lib/python2.7/site-packages")))
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
     `(("gcc" ,gcc-7)                      ;7 or later is required
       ("gperf" ,gperf)
       ("pkg-config" ,pkg-config)
       ("python-cython" ,python-cython)
       ("python-sphinx" ,python-sphinx)
       ("yasm" ,yasm)

       ;; For tests.
       ("inetutils" ,inetutils)
       ("jq" ,jq)
       ("perl" ,perl)
       ("xmlstarlet" ,xmlstarlet)
       ("python2-cram" ,python2-cram)
       ("python2-virtualenv" ,python2-virtualenv)

       ;; These dependencies are taken from test-requirements.txt
       ;; of ceph-disk and ceph-detect-init. The latter can also
       ;; test against python3, but let's try to get python2 tests
       ;; working first since that is the default.
       ("python2-configobj" ,python2-configobj)
       ("python2-coverage" ,python2-coverage)
       ("python2-discover" ,python2-discover)
       ("python2-fixtures" ,python2-fixtures)
       ("python2-flake8" ,python2-flake8)
       ("python2-mock" ,python2-mock)
       ("python2-nose" ,python2-nose)
       ("python2-pip" ,python2-pip)
       ("python2-pytest" ,python2-pytest)
       ("python2-subunit" ,python2-subunit)
       ("python2-testrepository" ,python2-testrepository)
       ("python2-testtools" ,python2-testtools)
       ("python2-tox" ,python2-tox)))
    (inputs
     `(("boost" ,boost)
       ("curl" ,curl)
       ("cryptsetup" ,cryptsetup)
       ("expat" ,expat)
       ("fcgi" ,fcgi)
       ("fuse" ,fuse)
       ("gptfdisk" ,gptfdisk)
       ("jemalloc" ,jemalloc)
       ("keyutils" ,keyutils)
       ("leveldb" ,leveldb)
       ("libaio" ,libaio)
       ("libatomic-ops" ,libatomic-ops)
       ("lua" ,lua)
       ("lz4" ,lz4)
       ("oath-toolkit" ,oath-toolkit)
       ("openldap" ,openldap)
       ("openssl" ,openssl)
       ("nss" ,nss)
       ("parted" ,parted)
       ("python@2" ,python-2)
       ("python2-prettytable" ,python2-prettytable)      ;used by ceph_daemon.py
       ("python2-six" ,python2-six)                      ;for ceph-mgr + plugins
       ("python@3" ,python-3)
       ("rapidjson" ,rapidjson)
       ("rocksdb" ,rocksdb)
       ("snappy" ,snappy)
       ("udev" ,eudev)
       ("util-linux" ,util-linux)
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
