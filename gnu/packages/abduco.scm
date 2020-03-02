;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
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

(define-module (gnu packages abduco)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public abduco
  (package
   (name "abduco")
   (version "0.6")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://www.brain-dump.org/projects/abduco/abduco-"
                  version ".tar.gz"))
            (sha256
             (base32
              "1x1m58ckwsprljgmdy93mvgjyg9x3cqrzdf3mysp0mx97zhhj2f9"))))
   (build-system gnu-build-system)
   (arguments
    `(#:make-flags (list "CC=gcc"
                         (string-append "PREFIX=" (assoc-ref %outputs "out")))
      #:phases (modify-phases %standard-phases
                 (delete 'configure)
                 (delete 'check)))) ; no test suite
   (synopsis "Session management in a clean and simple way")
   (description "abduco provides session management i.e. it allows programs to
be run independently from their controlling terminal.  That is, programs can
be detached---run in the background---and then later reattached.")
   (home-page "https://www.brain-dump.org/projects/abduco/")
   (license isc)))
