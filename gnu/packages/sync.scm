;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls))

(define-public owncloud-client
  (package
    (name "owncloud-client")
    (version "2.5.1.10973")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.owncloud.com/desktop/stable/"
                           "owncloudclient-" version ".tar.xz"))
       (sha256
        (base32 "19x4rbnqg7f7hspz1xy86b1q51q1n5y7yvq8kqc1m64n2r2s3srk"))
       (patches (search-patches "owncloud-disable-updatecheck.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; libcrashreporter-qt has its own bundled dependencies
           (delete-file-recursively "src/3rdparty/libcrashreporter-qt")
           (delete-file-recursively "src/3rdparty/sqlite3")
           ;; qprogessindicator, qlockedfile, qtokenizer and
           ;; qtsingleapplication have not yet been packaged, but all are
           ;; explicitly used from the 3rdparty folder during build.
           ;; We can also remove the macgoodies folder
           (delete-file-recursively "src/3rdparty/qtmacgoodies")
           #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-failing-tests
           ;; "Could not create autostart folder"
           (lambda _
             (substitute* "test/CMakeLists.txt"
                          (("owncloud_add_test\\(Utility \"\"\\)" test)
                           (string-append "#" test)))
             #t))
         (add-after 'unpack 'dont-embed-store-path
           (lambda _
             (substitute* "src/common/utility_unix.cpp"
               (("QCoreApplication::applicationFilePath\\()") "\"owncloud\""))
             #t))
         (delete 'patch-dot-desktop-files))
       #:configure-flags '("-DUNIT_TESTING=ON"
                           ;; build without qtwebkit, which causes the
                           ;; package to FTBFS while looking for QWebView.
                           "-DNO_SHIBBOLETH=1")))
    (native-inputs
     `(("cmocka" ,cmocka)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("qtlinguist" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtkeychain" ,qtkeychain)
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
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/sieren/QSyncthingTray")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1n9g4j7qznvg9zl6x163pi9f7wsc3x6q76i33psnm7x2v1i22x5w"))))
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
               (mkdir-p (string-append out "/share/pixmaps"))
               (copy-file "../source/resources/images/Icon1024.png"
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
    (version "2.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/axkibe/lsyncd.git")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q2ixp52r96ckghgmxdbms6xrq8dbziimp8gmgzqfq4lk1v1w80y"))))
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
               (install-file "../source/doc/manpage/lsyncd.1" man)
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
comparatively easy to install not requiring new file systems or block devices
and does not hamper local file system performance.")
    (license license:gpl2+)))

(define-public casync
  (package
    (name "casync")
    (version "2")
    (home-page "https://github.com/systemd/casync/")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0znkp3fcksrykcsv06y2mjvf2lbwmin25snmvfa8i5qfm3f4rm88"))
              (file-name (string-append name "-" version "-checkout"))
              (patches (search-patches "casync-renameat2-declaration.patch"))))
    (build-system meson-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-sphinx" ,python-sphinx)
       ("rsync" ,rsync)))                         ;for tests
    (inputs
     `(("xz" ,xz)                                 ;for liblzma
       ("zstd" ,zstd)
       ("curl" ,curl)
       ("acl" ,acl)
       ("libselinux" ,libselinux)
       ("fuse" ,fuse)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (synopsis "File synchronization and backup system")
    (description
     "casync is a @dfn{content-addressable data synchronizer} that can be used
as the basis of a backup system.  It is:

@itemize
@item A combination of the rsync algorithm and content-addressable storage;
@item An efficient way to store and retrieve multiple related versions of
large file systems or directory trees;
@item An efficient way to deliver and update OS, VM, IoT and container images
over the Internet in an HTTP and CDN friendly way;
@item An efficient backup system.
@end itemize\n")
    (license license:lgpl2.1+)))

(define-public rclone
  (package
    (name "rclone")
    (version "1.46")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ncw/rclone.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fl52dl41n76r678nzkxa2kgk9khn1fxraxgk8jd3ayc787qs9ia"))))
    ;; FIXME: Rclone bundles some libraries Guix already provides.  Need to
    ;; un-bundle them.
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/ncw/rclone"
       #:install-source? #f))
    (synopsis "@code{rsync} for cloud storage")
    (description "@code{Rclone} is a command line program to sync files and
directories to and from different cloud storage providers.

Features include:
@itemize
@item MD5/SHA1 hashes checked at all times for file integrity
@item Timestamps preserved on files
@item Partial syncs supported on a whole file basis
@item Copy mode to just copy new/changed files
@item Sync (one way) mode to make a directory identical
@item Check mode to check for file hash equality
@item Can sync to and from network, e.g., two different cloud accounts
@item Optional encryption (Crypt)
@item Optional cache (Cache)
@item Optional FUSE mount (rclone mount)
@end itemize")
    (home-page "https://rclone.org/")
    (license license:expat)))
