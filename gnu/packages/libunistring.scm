;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu packages libunistring)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base))

(define-public libunistring
  (package
   (name "libunistring")
   (version "0.9.10")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://gnu/libunistring/libunistring-"
                  version ".tar.xz"))
            (sha256
             (base32
              "1mq57h06622m6qc5cv347fc3qk5mj840axw3c0vd7qmnwk1v53zb"))))
   (propagated-inputs (libiconv-if-needed))
   (outputs '("out" "static"))
   (build-system gnu-build-system)
   (arguments
    ;; Work around parallel build issue whereby C files may be compiled before
    ;; config.h is built: see <http://hydra.gnu.org/build/59381/nixlog/2/raw> and
    ;; <http://lists.openembedded.org/pipermail/openembedded-core/2012-April/059850.html>.
    '(#:parallel-build? #f
      #:phases (modify-phases %standard-phases
                 (add-after 'install 'move-static-library
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let ((out (assoc-ref outputs "out"))
                           (static (assoc-ref outputs "static")))
                       (with-directory-excursion (string-append out "/lib")
                         (install-file "libunistring.a"
                                       (string-append static "/lib"))
                         (delete-file "libunistring.a")
                         #t)))))))
   (synopsis "C library for manipulating Unicode strings")
   (description
    "GNU libunistring is a library providing functions to manipulate
Unicode strings and for manipulating C strings according to the Unicode
standard.")
   (home-page "https://www.gnu.org/software/libunistring/")
   (license (list lgpl3+ gpl2))))
