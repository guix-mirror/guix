;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages sync)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls))

(define-public owncloud-client
  (package
    (name "owncloud-client")
    (version "2.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.owncloud.com/desktop/stable/"
                           "owncloudclient-" version ".tar.xz"))
       (sha256
        (base32 "1fpi1mlp2b8sx2993b4mava5c6qw794dmlayih430299z1l9wh49"))
       (patches (search-patches "owncloud-disable-updatecheck.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; only allows bundled libcrashreporter-qt
           (delete-file-recursively "src/3rdparty/libcrashreporter-qt")
           ;; we already package qtkeychain and sqlite
           (delete-file-recursively "src/3rdparty/qtkeychain")
           (delete-file-recursively "src/3rdparty/sqlite3")
           ;; qjson is packaged, qprogessindicator, qlockedfile, qtokenizer and
           ;; qtsingleapplication have not yet been packaged, but all are
           ;; explicitly used from the 3rdparty folder during build.
           ;; We can also remove the macgoodies folder
           (delete-file-recursively "src/3rdparty/qtmacgoodies")))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-rpath-dirs
          (lambda _
            (substitute* '("src/libsync/CMakeLists.txt"
                           "csync/src/CMakeLists.txt")
              ;; We store the libs in out/lib and not /usr/lib/appname, so we
              ;; need the executable to point to the libraries in /lib and not
              ;; in /lib/appname.
              (("\\/\\$\\{APPLICATION_EXECUTABLE\\}") ""))
            (substitute* '("src/cmd/CMakeLists.txt"
                           "src/crashreporter/CMakeLists.txt"
                           "src/gui/CMakeLists.txt")
              ;; This has the same issue as the substitution above.
              (("\\/\\$\\{APPLICATION_EXECUTABLE\\}\\\"") "\""))
            #t))
         (add-after 'unpack 'delete-failing-tests
           ;; "Could not create autostart folder"
           (lambda _
             (substitute* "test/CMakeLists.txt"
                          (("owncloud_add_test\\(Utility \"\"\\)" test)
                           (string-append "#" test)))
             #t)))
       #:configure-flags '("-DUNIT_TESTING=ON")))
    (native-inputs
     `(("cmocka" ,cmocka)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("qtlinguist" ,qttools)))
    (inputs
     `(("inotify-tools" ,inotify-tools)
       ("openssl" ,openssl)
       ("qtbase" ,qtbase)
       ("qtkeychain" ,qtkeychain)
       ("qtwebkit" ,qtwebkit)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (home-page "https://owncloud.org")
    (synopsis "Folder synchronization with an ownCloud server")
    (description "The ownCloudSync system lets you always have your latest
files wherever you are.  Just specify one or more folders on the local machine
to and a server to synchronize to.  You can configure more computers to
synchronize to the same server and any change to the files on one computer will
silently and reliably flow across to every other.")
    (license license:gpl2+)))

(define-public qsyncthingtray
  (package
    (name "qsyncthingtray")
    (version "0.5.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://github.com/sieren/QSyncthingTray/archive/"
               version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1024778ml7q62ziqm4d22z5sc1715l34846pwfyzfpcyl32qlhpz"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DQST_BUILD_WEBKIT=1")
       #:phases
       (modify-phases %standard-phases
         ;; The program is meant to be run from the git repo or source tarball.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "QSyncthingTray" bin)
               (install-file (string-append
                               "../QSyncthingTray-"
                               ,(package-version qsyncthingtray)
                               "/resources/images/Icon1024.png")
                             (string-append
                               out "/share/pixmaps/QSyncthingTray.png"))
               #t))))
       #:tests? #f)) ; no test target
    (inputs
     `(("qtbase" ,qtbase)
       ("qtwebkit" ,qtwebkit)))
    (home-page "https://github.com/sieren/QSyncthingTray")
    (synopsis "Traybar Application for Syncthing")
    (description
     "A traybar application for syncthing.
@enumerate
@item Shows number of connections at a glance.
@item Traffic statistics about incoming, outgoing and total throughput.
@item Launches Syncthing and Syncthing-iNotifier if specified.
@item Quickly pause Syncthing with one click.
@item Last Synced Files - Quickly see the recently synchronised files and open
their folder.
@item Quick Access to all shared folders.
@item Presents Syncthing UI in a separate view instead of using the browser.
@item Supports authenticated HTTPS connections.
@item Uses System Notifications about current connection status.
@item Toggle for monochrome icon.
@end enumerate\n")
    (license license:lgpl3+)))

(define-public lsyncd
  (package
    (name "lsyncd")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/axkibe/lsyncd/archive/release-"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0da7mrq2c578v2dd5x9v75l1fqrm28jvn28qkcd49y8p992nj6gl"))))
    (build-system cmake-build-system)
    (arguments
     `(;; The "tests" target is broken and assumes that tests are run in the
       ;; root directory.
       #:tests? #f
       #:test-target "tests"
       #:phases
       (modify-phases %standard-phases
         ;; No install target.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (install-file "lsyncd" bin)
               (install-file (string-append "../lsyncd-release-"
                                            ,version
                                            "/doc/manpage/lsyncd.1")
                             man)
               #t))))))
    (native-inputs
     `(("lua" ,lua-5.2)))
    (home-page "https://github.com/axkibe/lsyncd")
    (synopsis "Synchronize local directories with remote targets")
    (description "Lsyncd watches a local directory trees event monitor
interface (inotify or fsevents).  It aggregates and combines events for a few
seconds and then spawns one (or more) process(es) to synchronize the changes.
By default this is rsync, which must be installed on all source and target
machines.  Lsyncd is thus a light-weight live mirror solution that is
comparatively easy to install not requiring new filesystems or block devices
and does not hamper local filesystem performance.")
    (license license:gpl2+)))
