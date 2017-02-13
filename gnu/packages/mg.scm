;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.org>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages mg)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config))

(define-public mg
  (package
    (name "mg")
    (version "20161005")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://homepage.boetes.org/software/mg/mg-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0qaydk2cy765n9clghmi5gdnpwn15y2v0fj6r0jcm0v7d89vbz5p"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "GNUmakefile"
                    (("/usr/bin/") ""))))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libbsd" ,libbsd)
       ("ncurses" ,ncurses)))
    (arguments
     ;; No test suite available.
     '(#:tests? #f
       #:make-flags (list (string-append "prefix=" %output)
                          "CURSES_LIBS=-lncurses"
                          "CC=gcc")
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-before 'install 'patch-tutorial-location
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* "mg.1"
                        (("/usr") (assoc-ref outputs "out")))
                      #t))
                  (add-after 'install 'install-tutorial
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (doc (string-append out "/share/doc/mg")))
                        (install-file "tutorial" doc)
                        #t))))))
    (home-page "http://homepage.boetes.org/software/mg/")
    (synopsis "Microscopic GNU Emacs clone")
    (description
     "Mg (mg) is a GNU Emacs style editor, with which it is \"broadly\"
compatible.  This is a portable version of the mg maintained by the OpenBSD
team.")
    (license public-domain)))
