;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (gnu packages acl)
  #:use-module (guix licenses)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages perl)
  #:use-module ((gnu packages gettext)
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
       (alist-cons-after
        'configure 'patch-makefile-SHELL
        (lambda _
          (patch-makefile-SHELL "include/buildmacros"))
        ,(if (%current-target-system)
             '%standard-cross-phases
             '(alist-replace 'check
                             (lambda _
                               (system* "make" "tests" "-C" "test")

                               ;; XXX: Ignore the test result since this is
                               ;; dependent on the underlying file system.
                               #t)
                             %standard-phases)))))
    (inputs `(("attr" ,attr)

              ;; Perl is needed to run tests; remove it from cross builds.
              ,@(if (%current-target-system)
                    '()
                    `(("gettext" ,guix:gettext)
                      ("perl" ,perl)))))
    (native-inputs
     ;; FIXME: Upon next core-updates, make gettext a native input
     ;; unconditionally.
     (if (%current-target-system)
         `(("gettext" ,guix:gettext))
         '()))

    (home-page
     "http://savannah.nongnu.org/projects/acl")
    (synopsis
     "Library and tools for manipulating access control lists")
    (description
     "Library and tools for manipulating access control lists.")
    (license (list gpl2+ lgpl2.1+))))
