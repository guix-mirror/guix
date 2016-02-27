;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages owncloud)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages tls))

(define-public owncloud-client
  (package
    (name "owncloud-client")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.owncloud.com/desktop/stable/"
                           "owncloudclient-" version ".tar.xz"))
       (sha256
        (base32 "1jxi439qff4acvyvszjprj42kvzhlz255wv8g4p3jrf55svzwz2f"))))
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
              (("\\/\\$\\{APPLICATION_EXECUTABLE\\}\\\"") "\"")))))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("inotify-tools" ,inotify-tools)
       ("openssl" ,openssl)
       ("perl" ,perl)
       ("python-wrapper" ,python-wrapper)
       ("qt" ,qt)
       ("qtkeychain" ,qtkeychain)
       ("ruby" ,ruby)
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
