;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages conkeror)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnuzilla))

(define-public conkeror
  (package
    (name "conkeror")
    (version "1.0pre1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "http://repo.or.cz/w/conkeror.git/snapshot/"
                              "8a26fff5896a3360549e2adfbf06b1d57e909266"
                              ".tar.gz")) ; tag: debian-1.0--pre-1+git140616-1
              (sha256
               (base32
                "1cgjzi7g3g22zcx6bpfnid4i12sb45w6icmxdzjn8d3c0m8qsyp1"))))
    (build-system gnu-build-system)
    (inputs `(("icecat" ,icecat)))
    (arguments
     `(#:tests? #f                      ;no tests
       #:make-flags '("CC=gcc")
       #:phases
       (alist-delete
        'configure
        (alist-replace
         'install
         (lambda _
           (begin
             (use-modules (guix build utils))
             (let* ((datadir  (string-append %output "/share/conkeror"))
                    (bindir   (string-append %output "/bin"))
                    (launcher (string-append bindir  "/conkeror"))
                    (spawn    (string-append bindir  "/conkeror-spawn-helper")))
               (copy-recursively "." datadir)
               (mkdir-p bindir)
               (copy-file "conkeror-spawn-helper" spawn)
               (call-with-output-file launcher
                 (lambda (p)
                   (format p "#!~a/bin/bash
exec ~a/bin/icecat --app ~a \"$@\"~%"
                           (assoc-ref %build-inputs "bash") ;implicit input
                           (assoc-ref %build-inputs "icecat")
                           (string-append datadir
                                          "/application.ini"))))
               (chmod launcher #o555))))
         %standard-phases))))
    (synopsis "Keyboard focused web browser with Emacs look and feel")
    (description "Conkeror is a highly-programmable web browser based on
Mozilla XULRunner which is the base of all Mozilla products including Firefox.
Conkeror has a sophisticated keyboard system for running commands and
interacting with web page content, modelled after Emacs and Lynx.  It is
self-documenting and extensible with JavaScript.

It comes with builtin support for several Web 2.0 sites like several Google
services (Search, Gmail, Maps, Reader, etc.), Del.icio.us, Reddit, Last.fm and
YouTube.  For easier editing of form fields, it can spawn external editors.")
    (home-page "http://conkeror.org")
    ;; Conkeror is triple licensed.
    (license (list license:gpl2
                   license:lgpl2.1
                   ;; MPL 1.1 -- this license is not GPL compatible
                   ))))
