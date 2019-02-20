;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.com>
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

(define-module (gnu packages simh)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages admin))

(define-public simh
  (package
    (name "simh")
    (version "3.9-0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/simh/simh.git")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1jiq6shj6a9xvzacvmyhxxd6xdyica8q4006qqjh5mh96rxrp15c"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (inputs
     `(("libpcap" ,libpcap)))
    (arguments
     '(#:tests? #f
       #:make-flags (list
                      "LDFLAGS=-lm"
                      (string-append "INCPATH="
                                     (assoc-ref %build-inputs "libpcap")
                                     "/include")
                      (string-append "LIBPATH="
                                     (assoc-ref %build-inputs "libpcap")
                                     "/lib"))
       #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-before 'build 'prepare-build
             (lambda _
               (mkdir "BIN")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin/"))
                      (lib (string-append out "/lib/simh/")))
                 (mkdir-p bin)
                 (mkdir-p lib)
                 (for-each
                   (lambda (file)
                     (copy-file file (string-append bin
                                                    "simh-"
                                                    (basename file))))
                   (find-files "BIN"))
                 (for-each
                   (lambda (file)
                     (copy-file file (string-append lib
                                                    (basename file))))
                   (find-files "VAX" "bin$"))))))))
    (home-page "http://simh.trailing-edge.com")
    (synopsis "Collection of simulators from The Computer History Simulation
Project")
    (description
     "SIMH is a highly portable, multi-system simulator.  SIMH implements
simulators for:

@itemize
@item Data General Nova, Eclipse.
@item Digital Equipment Corporation PDP-1, PDP-4, PDP-7, PDP-8, PDP-9, PDP-10,
PDP-11, PDP-15, VAX.
@item GRI Corporation GRI-909, GRI-99.
@item IBM 1401, 1620, 1130, 7090/7094, System 3.
@item Interdata (Perkin-Elmer) 16b and 32b systems.
@item Hewlett-Packard 2114, 2115, 2116, 2100, 21MX, 1000.
@item Honeywell H316/H516.
@item MITS Altair 8800, with both 8080 and Z80.
@item Royal-Mcbee LGP-30, LGP-21.
@item Scientific Data Systems SDS 940.
@item SWTP 6800.
@end itemize")
    (license license:expat)))
