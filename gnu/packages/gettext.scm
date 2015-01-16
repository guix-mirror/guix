;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (gnu packages gettext)
  #:use-module ((guix licenses) #:select (gpl3))
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages xml))

;; Use that name to avoid clashes with Guile's 'gettext' procedure.
;;
;; We used to resort to #:renamer on the user side, but that prevented
;; circular dependencies involving (gnu packages gettext).  This is because
;; 'resolve-interface' (as of Guile 2.0.9) iterates eagerly over the used
;; module when there's a #:renamer, and that module may be empty at that point
;; in case or circular dependencies.
(define-public gnu-gettext
  (package
    (name "gettext")
    (version "0.19.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/gettext/gettext-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0gvz86m4cs8bdf3mwmwsyx6lrq4ydfxgadrgd9jlx32z3bnz3jca"))))
    (build-system gnu-build-system)
    (inputs
     `(("expat" ,expat)))
    (arguments
     `(#:phases (alist-cons-before
                 'check 'patch-tests
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let* ((bash (which "sh")))
                     (substitute*
                         (find-files "gettext-tools/tests"
                                     "^(lang-sh|msg(exec|filter)-[0-9])")
                       (("#![[:blank:]]/bin/sh")
                        (format #f "#!~a" bash)))

                     (substitute* (cons "gettext-tools/src/msginit.c"
                                        (find-files "gettext-tools/gnulib-tests"
                                                    "posix_spawn"))
                       (("/bin/sh")
                        bash))

                     (substitute* "gettext-tools/src/project-id"
                       (("/bin/pwd")
                        "pwd"))))
                 %standard-phases)

       ;; When tests fail, we want to know the details.
       #:make-flags '("VERBOSE=yes")))
    (home-page "http://www.gnu.org/software/gettext/")
    (synopsis "Tools and documentation for translation")
    (description
     "GNU Gettext is a package providing a framework for translating the
textual output of programs into multiple languages.  It provides translators
with the means to create message catalogs, as well as an Emacs mode to work
with them, and a runtime library to load translated messages from the
catalogs.  Nearly all GNU packages use Gettext.")
    (license gpl3))) ; some files are under GPLv2+
