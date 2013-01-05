;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
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

(define-module (distro packages libffi)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public libffi
  (let ((post-install-phase
         ;; Install headers in the right place.
         '(lambda* (#:key outputs #:allow-other-keys)
            (define out (assoc-ref outputs "out"))
            (mkdir (string-append out "/include"))
            (with-directory-excursion
                (string-append out "/lib/libffi-3.0.9/include")
              (for-each (lambda (h)
                          (format #t "moving `~a' to includedir~%" h)
                          (rename-file h (string-append out "/include/" h)))
                        (scandir "."
                                 (lambda (x)
                                   (not (member x '("." ".."))))))))))
   (package
    (name "libffi")
    (version "3.0.9")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "ftp://sourceware.org/pub/libffi/"
                             name "-" version ".tar.gz"))
             (sha256
              (base32
               "0ln4jbpb6clcsdpb9niqk0frgx4k0xki96wiv067ig0q4cajb7aq"))))
    (build-system gnu-build-system)
    (arguments `(#:modules ((guix build utils) (guix build gnu-build-system)
                            (ice-9 ftw) (srfi srfi-26))
                 #:phases (alist-cons-after 'install 'post-install
                                            ,post-install-phase
                                            %standard-phases)))
    (synopsis "libffi, a foreign function call interface library")
    (description
     "The libffi library provides a portable, high level programming interface
to various calling conventions.  This allows a programmer to call any
function specified by a call interface description at run-time.

FFI stands for Foreign Function Interface.  A foreign function interface is
the popular name for the interface that allows code written in one language
to call code written in another language.  The libffi library really only
provides the lowest, machine dependent layer of a fully featured foreign
function interface.  A layer must exist above libffi that handles type
conversions for values passed between the two languages.")
    (home-page "http://sources.redhat.com/libffi/")

    ;; See <http://github.com/atgreen/libffi/blob/master/LICENSE>.
    (license expat))))

