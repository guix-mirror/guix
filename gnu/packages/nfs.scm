;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2017, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages nfs)
  #:use-module (gnu packages)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sqlite)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define-public nfs-utils
  (package
    (name "nfs-utils")
    (version "2.4.2")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://kernel.org/linux/utils/nfs-utils/" version
                   "/nfs-utils-" version ".tar.xz"))
             (sha256
              (base32
               "0f0hm8jq1p5gra55v621qpbb3mryakaikzpy5znkvxym0dx76r24"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       `("--without-tcp-wrappers"
         ,(string-append "--with-start-statd="
                         (assoc-ref %outputs "out") "/sbin/start-statd")
         ,(string-append "--with-krb5=" (assoc-ref %build-inputs "mit-krb5"))
         ,(string-append "--with-pluginpath="
                         (assoc-ref %outputs "out")
                         "/lib/libnfsidmap")
         "--enable-svcgss")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'adjust-command-file-names
           (lambda _
             ;; Remove assumptions of FHS from start-statd script
             (substitute* `("utils/statd/start-statd")
               (("^PATH=.*") "")
               (("^flock")
                (string-append
                 (assoc-ref %build-inputs "util-linux")
                 "/bin/flock"))
               (("^exec rpc.statd")
                (string-append "exec "
                 (assoc-ref %outputs "out") "/sbin/rpc.statd")))

             ;; find rpcgen
             (substitute* "configure"
               (("/usr/local/bin/rpcgen")
                (which "rpcgen")))

             ;; This hook tries to write to /var
             ;; That needs to be done by a service too.
             (substitute* `("Makefile.in")
               (("^install-data-hook:")
                "install-data-hook-disabled-for-guix:"))

             ;; Replace some hard coded paths.
             (substitute* `("utils/nfsd/nfssvc.c")
               (("/bin/mount")
                (string-append
                 (assoc-ref %build-inputs "util-linux")
                 "/bin/mount")))
             (substitute* `("utils/statd/statd.c")
               (("/usr/sbin/")
                (string-append (assoc-ref %outputs "out") "/sbin/")))
             (substitute* `("utils/mount/Makefile.in"
                            "utils/nfsdcld/Makefile.in"
                            "utils/nfsdcltrack/Makefile.in")
               (("^sbindir = /sbin")
                (string-append "sbindir = "
                               (assoc-ref %outputs "out") "/sbin")))
             #t)))))
    (inputs
     `(("keyutils" ,keyutils)
       ("libevent" ,libevent)
       ("libnfsidmap" ,libnfsidmap)
       ("rpcsvc-proto" ,rpcsvc-proto) ;for 'rpcgen'
       ("sqlite" ,sqlite)
       ("lvm2" ,lvm2)
       ("util-linux" ,util-linux "lib")
       ("mit-krb5" ,mit-krb5)
       ("libtirpc" ,libtirpc)
       ("python-wrapper" ,python-wrapper))) ;for the Python based tools
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.kernel.org/pub/linux/utils/nfs-utils/")
    (synopsis "Tools for loading and managing Linux NFS mounts")
    (description "The Network File System (NFS) was developed to allow
machines to mount a disk partition on a remote machine as if it were a local
disk.  It allows for fast, seamless sharing of files across a network.")
    ;; It is hard to be sure what the licence is.  Most of the source files
    ;; contain no licence notice at all.  A few have a licence notice for a 3
    ;; clause non-copyleft licence.  However the tarball has a COPYING file
    ;; with the text of GPLv2 -- It seems then that GLPv2 is the most
    ;; restrictive licence, and until advice to the contrary we must assume
    ;; that is what is intended.
    (license license:gpl2)))
