;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Matthew Jordan <matthewjordandevops@yandex.com>
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages entr)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public entr
  (package
    (name "entr")
    (version "4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://entrproject.org/code/entr-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0w2xkf77jikcjh15fp9g7661ss30pz3jbnh261vqpaqavwah4c17"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "CONFIG_SHELL" (which "bash"))
               (setenv "CC" (which "gcc"))
               (setenv "DESTDIR" (string-append out "/"))
               (setenv "PREFIX" "")
               (setenv "MANPREFIX" "man")
               (invoke "./configure"))))
         (add-before 'build 'remove-fhs-file-names
           (lambda _
             ;; Use the tools available in $PATH.
             (substitute* "entr.c"
               (("/bin/cat") "cat")
               (("/usr/bin/clear") "clear"))
             #t)))))
    (home-page "http://entrproject.org/")
    (synopsis "Run arbitrary commands when files change")
    (description
     "entr is a zero-configuration tool with no external build or run-time
dependencies.  The interface to entr is not only minimal, it aims to be simple
enough to create a new category of ad hoc automation.  These micro-tests
reduce keystrokes, but more importantly they emphasize the utility of
automated checks.")

    ;; Per 'LICENSE', portability code under missing/ is under BSD-2.
    (license isc)))
