;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017, 2020, 2021 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
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

(define-module (gnu packages ci)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system gnu))

(define-public cuirass
  (let ((commit "1b35a7785627f3e8c5b6d8f37bf11eb4a470b4c4")
        (revision "7"))
    (package
      (name "cuirass")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.savannah.gnu.org/git/guix/guix-cuirass.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1hy8inhwpi9i92yg5alvmqgp1vjnrhinckywkv9kgyjlskzshib3"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build utils)
                    (guix build gnu-build-system)
                    (ice-9 rdelim)
                    (ice-9 popen))
         #:configure-flags '("--localstatedir=/var") ;for /var/log/cuirass
         #:parallel-tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-before 'bootstrap 'fix-version-gen
             (lambda _
              (patch-shebang "build-aux/git-version-gen")

              (call-with-output-file ".tarball-version"
                (lambda (port)
                  (display ,version port)))))
           (add-before 'check 'set-PATH-for-tests
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((pg (assoc-ref inputs "ephemeralpg"))
                     (path (getenv "PATH")))
                 (setenv "PATH" (string-append pg "/bin:" path))
                 #t)))
           ;; Disable the remote tests that require a Guix daemon connection.
           (add-before 'check 'disable-remote-tests
             (lambda _
               (substitute* "Makefile.am"
                 (("tests/remote.scm") ""))
               #t))
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; Wrap the 'cuirass' command to refer to the right modules.
               (let* ((out    (assoc-ref outputs "out"))
                      (avahi  (assoc-ref inputs "guile-avahi"))
                      (gcrypt (assoc-ref inputs "guile-gcrypt"))
                      (json   (assoc-ref inputs "guile-json"))
                      (zmq    (assoc-ref inputs "guile-simple-zmq"))
                      (squee  (assoc-ref inputs "guile-squee"))
                      (git    (assoc-ref inputs "guile-git"))
                      (bytes  (assoc-ref inputs "guile-bytestructures"))
                      (fibers (assoc-ref inputs "guile-fibers"))
                      (zlib   (assoc-ref inputs "guile-zlib"))
                      (matd   (assoc-ref inputs "guile-mastodon"))
                      (tls    (assoc-ref inputs "gnutls"))
                      (mail   (assoc-ref inputs "mailutils"))
                      (guix   (assoc-ref inputs "guix"))
                      (deps   (list avahi gcrypt json zmq squee git bytes
                                    fibers zlib matd tls mail guix))
                      (guile  (assoc-ref %build-inputs "guile"))
                      (effective
                       (read-line
                        (open-pipe* OPEN_READ
                                    (string-append guile "/bin/guile")
                                    "-c" "(display (effective-version))")))
                      (mods
                       (string-drop-right  ;drop trailing colon
                        (string-join deps
                                     (string-append "/share/guile/site/"
                                                    effective ":")
                                     'suffix)
                        1))
                      (objs
                       (string-drop-right
                        (string-join deps
                                     (string-append "/lib/guile/" effective
                                                    "/site-ccache:")
                                     'suffix)
                        1)))
                 ;; Make sure 'cuirass' can find the relevant Guile modules.
                 (wrap-program (string-append out "/bin/cuirass")
                   `("PATH" ":" prefix (,(string-append out "/bin")))
                   `("GUILE_LOAD_PATH" ":" prefix (,mods))
                   `("GUILE_LOAD_COMPILED_PATH" ":" prefix (,objs)))
                 #t))))))
      (inputs
       `(("guile" ,guile-3.0/libgc-7)
         ("guile-avahi" ,guile-avahi)
         ("guile-fibers" ,guile-fibers)
         ("guile-gcrypt" ,guile-gcrypt)
         ("guile-json" ,guile-json-4)
         ("guile-simple-zmq" ,guile-simple-zmq)
         ("guile-squee" ,guile-squee)
         ("guile-git" ,guile-git)
         ("guile-zlib" ,guile-zlib)
         ("guile-mastodon" ,guile-mastodon)
         ("gnutls" ,gnutls)
         ("mailutils" ,mailutils)
         ;; FIXME: this is propagated by "guile-git", but it needs to be among
         ;; the inputs to add it to GUILE_LOAD_PATH.
         ("guile-bytestructures" ,guile-bytestructures)
         ("guix" ,guix)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("pkg-config" ,pkg-config)
         ("texinfo" ,texinfo)
         ("ephemeralpg" ,ephemeralpg)))
      (native-search-paths
       ;; For HTTPS access, Cuirass itself honors these variables, with the
       ;; same semantics as Git and OpenSSL (respectively).
       (list (search-path-specification
              (variable "GIT_SSL_CAINFO")
              (file-type 'regular)
              (separator #f)                      ;single entry
              (files '("etc/ssl/certs/ca-certificates.crt")))
             (search-path-specification
              (variable "SSL_CERT_DIR")
              (files '("etc/ssl/certs")))))
      (synopsis "Continuous integration system")
      (description
       "Cuirass is a continuous integration tool using GNU Guix.  It is
intended as a replacement for Hydra.")
      (home-page "https://guix.gnu.org/cuirass/")
      (license l:gpl3+))))
