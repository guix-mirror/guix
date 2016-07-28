;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Claes Wallin <claes.wallin@greatsinodevelopment.com>
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

(define-module (gnu packages skarnet)
  #:use-module (gnu packages)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public skalibs
  (package
    (name "skalibs")
    (version "2.3.10.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://skarnet.org/software/skalibs/skalibs-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0i7af224kl1crxgml09wx0x6q8ab79vnyrllfwv2lnq585wi9mg4"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-force-devr") ; do not analyze /dev/random
       #:tests? #f)) ; no tests exist
    (home-page "http://skarnet.org/software/skalibs/")
    (synopsis "Platform abstraction libraries for skarnet.org software")
    (description
     "This package provides lightweight C libraries isolating the developer
from portability issues, providing a unified systems API on all platforms,
including primitive data types, cryptography, and POSIX concepts like sockets
and file system operations.  It is used by all skarnet.org software.")
    (license isc)))

(define-public execline
  (package
    (name "execline")
    (version "2.1.5.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://skarnet.org/software/execline/execline-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0hhirdmyh3sj9qagkis7addmmdvyic717wkb6ym3n63kvfk0adla"))))
    (build-system gnu-build-system)
    (inputs `(("skalibs" ,skalibs)))
    (arguments
     '(#:configure-flags (list
                          (string-append "--with-lib="
                                         (assoc-ref %build-inputs "skalibs")
                                         "/lib/skalibs")
                          (string-append "--with-sysdeps="
                                         (assoc-ref %build-inputs "skalibs")
                                         "/lib/skalibs/sysdeps"))
       #:phases (modify-phases %standard-phases
                  (add-after
                   'install 'post-install
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (bin (string-append out "/bin")))
                      (wrap-program (string-append bin "/execlineb")
                        `("PATH" ":" prefix (,bin)))))))
       #:tests? #f)) ; No tests exist.
    (home-page "http://skarnet.org/software/execline/")
    (license isc)
    (synopsis "Non-interactive shell-like language with minimal overhead")
    (description
     "Execline is a (non-interactive) scripting language, separated into a
parser (execlineb) and a set of commands meant to execute one another in a
chain-execution fashion, storing the whole script in the argument array.
It features conditional loops, getopt-style option handling, file name
globbing, redirection and other shell concepts, expressed as discrete commands
rather than in special syntax, minimizing runtime footprint and
complexity.")))
