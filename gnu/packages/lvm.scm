;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages lvm)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config))

(define-public lvm2
  (package
   (name "lvm2")
   (version "2.02.98")
   (source (origin
            (method url-fetch)
            (uri (string-append "ftp://sources.redhat.com/pub/lvm2/LVM2."
                                version ".tgz"))
            (sha256
             (base32
              "0r6q6z8ip6q5qgkzng0saljassp4912k6i21ra10vq7pzrc0l0vi"))))
   (build-system gnu-build-system)
   (native-inputs
     `(("pkg-config" ,pkg-config)
       ("procps" ,procps)))
   (arguments
    `(#:tests? #f ; require to be root
      #:configure-flags
       `(,(string-append "--with-confdir=" (assoc-ref %outputs "out") "/etc"))
      #:phases
       (alist-cons-before
        'configure 'patch-make-tmpl
        (lambda _
          (substitute* "make.tmpl.in"
            (("/bin/sh") (which "sh"))
            (("CC \\?=") "CC ="))) ; force CC argument to be set from configure
         %standard-phases)))
   (synopsis "logical volume management")
   (description
    "LVM2 refers to the userspace toolset that provides logical volume
management facilities on linux. It is reasonably backwards-compatible with
the original LVM toolset.")
   (license license:gpl2)
   (home-page "http://www.sourceware.org/lvm2/")))
