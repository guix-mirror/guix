;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages toys)
  #:use-module (gnu packages ncurses)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public sl
  (package
    (name "sl")
    (version "5.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mtoyoda/" name
                           "/archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fjnnnxxq7zh9bm3yzbj84fgap0rhblxi2m10br83747gxsrcn8y"))))
    (build-system gnu-build-system)
    (inputs
     `(("ncurses" ,ncurses)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (delete 'check)                ; no tests
         (replace 'install              ; no ‘make install’ target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man"))
                    (man1 (string-append man "/man1"))
                    (man1-ja (string-append man "/ja/man1")))
               (install-file "sl" bin)
               (install-file "sl.1" man1)
               (mkdir-p man1-ja)
               (copy-file "sl.1.ja" (string-append man1-ja "/sl.1"))
               #t))))))
    (home-page "http://www.tkl.iis.u-tokyo.ac.jp/~toyoda/index_e.html")
    (synopsis "Joke command to correct typing \"sl\" by mistake")
    (description
     "@dfn{SL} (for Steam Locomotive) displays one of several animated trains
on the text terminal.  It serves no useful purpose but to discourage mistakenly
typing @command{sl} instead of @command{ls}.")
    (license (license:non-copyleft "file://LICENSE"
                                   "See LICENSE in the distribution."))))
