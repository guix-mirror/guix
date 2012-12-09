;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (distro packages acl)
  #:use-module (distro packages attr)
  #:use-module (distro packages perl)
  #:use-module ((distro packages gettext)
                #:renamer (symbol-prefix-proc 'guix:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public acl
  (package
    (name "acl")
    (version "2.2.51")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://savannah/acl/acl-"
                          version ".src.tar.gz"))
      (sha256
       (base32
        "09aj30m49ivycl3irram8c3givc0crivjm3ymw0nhfaxrwhlb186"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (alist-replace 'check
                      (lambda _
                        (patch-shebang "test/run")
                        (system* "make" "tests" "-C" "test")

                        ;; XXX: Ignore the test result since this is
                        ;; dependent on the underlying file system.
                        #t)
                      %standard-phases)))
    (inputs `(("attr" ,attr)
              ("gettext" ,guix:gettext)
              ("perl" ,perl)))
    (home-page
     "http://savannah.nongnu.org/projects/acl")
    (synopsis
     "Library and tools for manipulating access control lists")
    (description
     "Library and tools for manipulating access control lists.")
    (license '("GPLv2+" "LGPLv2.1+"))))
