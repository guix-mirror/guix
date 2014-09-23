;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages boost)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tcsh)
  #:use-module (gnu packages perl))

(define-public boost
  (package
    (name "boost")
    (version "1.55.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/boost/boost_"
                    (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                    ".tar.bz2"))
              (sha256
               (base32
                "0lkv5dzssbl5fmh2nkaszi8x9qbj80pr4acf9i26sj3rvlih1w7z"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2)
       ("tcsh" ,tcsh)))
    (arguments
     (let ((build-flags
            `("threading=multi" "link=shared"
              ;; Boost's 'context' library is not yet supported on mips64, so
              ;; we disable it.  The 'coroutine' library depends on 'context',
              ;; so we disable that too.
              ,@(if (equal? "mips64el-linux" (or (%current-target-system)
                                                 (%current-system)))
                    '("--without-context" "--without-coroutine")
                    '()))))
       `(#:phases
         (alist-replace
          'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (substitute* '("libs/config/configure"
                             "libs/spirit/classic/phoenix/test/runtest.sh"
                             "tools/build/v2/doc/bjam.qbk"
                             "tools/build/v2/engine/execunix.c"
                             "tools/build/v2/engine/Jambase"
                             "tools/build/v2/engine/jambase.c")
                (("/bin/sh") (which "sh")))

              (setenv "SHELL" (which "sh"))
              (setenv "CONFIG_SHELL" (which "sh"))

              (zero? (system* "./bootstrap.sh"
                              (string-append "--prefix=" out)
                              "--with-toolset=gcc"))))
          (alist-replace
           'build
           (lambda _
             (zero? (system* "./b2" ,@build-flags)))

           (alist-replace
            'check
            (lambda _ #t)

            (alist-replace
             'install
             (lambda _
               (zero? (system* "./b2" "install" ,@build-flags)))
             %standard-phases)))))))

    (home-page "http://boost.org")
    (synopsis "Peer-reviewed portable C++ source libraries")
    (description
     "A collection of libraries intended to be widely useful, and usable
across a broad spectrum of applications.")
    (license (license:x11-style "http://www.boost.org/LICENSE_1_0.txt"
                                "Some components have other similar licences."))))
