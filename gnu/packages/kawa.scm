;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages kawa)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages java))

(define-public kawa
  (package
    (name "kawa")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/kawa/kawa-" version ".tar.gz"))
       (sha256
        (base32 "06g015zjlfgsx0n4lb326czkbf1grlx0n6dx074m808hdg6m16lc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f
       #:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-kawa
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/share/kawa/bin/kawa")
                 `("JAVA_HOME" ":" = (,(assoc-ref inputs "icedtea"))))
               #t))))))
    (inputs
     (list `(,icedtea-8 "jdk")))
    (home-page "https://www.gnu.org/software/kawa/")
    (synopsis "Java framework and implementation of Scheme, Elisp, and more")
    (description
     "GNU Kawa is an implementation of the Scheme programming language that
is built on top of the Java platform.  It is thus conveniently integrated
with Java and benefits from this by having a compiler, optional static
typing, and so on.  Kawa also serves as a framework for implementing other
programming languages on the Java platform.  Included in Kawa is qexo, a
partial implementation of XQuery in Java.")
    (license license:expat)))
