;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Simon South <simon@simonsouth.net>
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

(define-module (gnu packages ragel)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base))

(define-public ragel
  (package
    (name "ragel")
    (version "6.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.colm.net/files/ragel/ragel-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0gvcsl62gh6sg73nwaxav4a5ja23zcnyxncdcdnqa2yjcpdnw5az"))))
    (build-system gnu-build-system)
    (arguments
     (if (target-aarch64?)
         '(#:phases
           (modify-phases %standard-phases
             (add-after 'unpack 'apply-char-signedness-fix
               ;; Apply a backported fix for aarch64-linux, where the C/C++
               ;; "char" type is unsigned by default.
               ;;
               ;; The patch is applied in this custom phase and not via the
               ;; "origin" object above to avoid rebuilding a large number of
               ;; packages on other platforms.
               (lambda _
                 (let ((patch
                        (search-input-file %build-inputs "/bin/patch"))
                       (char-signedness-patch
                        (assoc-ref %build-inputs "char-signedness-patch")))
                   (invoke patch "-p1" "-i" char-signedness-patch))))))
         '()))
    (native-inputs
     (if (target-aarch64?)
         `(("char-signedness-patch"
            ,(search-patch "ragel-char-signedness.patch"))
           ("patch" ,patch))
         '()))
    (home-page "https://www.colm.net/open-source/ragel/")
    (synopsis "State machine compiler")
    (description
     "Ragel compiles executable finite state machines from regular languages.
Ragel targets C, C++, Obj-C, C#, D, Java, Go and Ruby.  Ragel state machines
can not only recognize byte sequences as regular expression machines do, but
can also execute code at arbitrary points in the recognition of a regular
language.  Code embedding is done using inline operators that do not disrupt
the regular language syntax.")
    ;; GPLv2 (or later) with exception for generated code.
    (license license:gpl2+)))

