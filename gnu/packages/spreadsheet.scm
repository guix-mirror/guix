;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2021 Jorge Gomez <jgart@dismail.de>
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

(define-module (gnu packages spreadsheet)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages base)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages ncurses))

(define-public sc-im
    (package
      (name "sc-im")
      (version "0.8.2")
      (home-page "https://github.com/andmarti1424/sc-im")
      (source (origin
                (method git-fetch)
                (uri
                  (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (sha256
                  (base32
                    "1nrjnw8sg75i0hkcbvjv7gydjddxjm27d5m1qczpg29fk9991q8z"))))
      (build-system gnu-build-system)
      (arguments
        ;; There are no tests at the moment.
        ;; https://github.com/andmarti1424/sc-im/issues/537
        ;; https://github.com/andmarti1424/sc-im/pull/385
        `(#:tests? #f
          #:make-flags (list "-C" "src"
                          (string-append "CC=" ,(cc-for-target))
                          (string-append "prefix=" %output))
          #:phases
            (modify-phases
               %standard-phases
                 (delete 'configure))))
      (inputs
        `(("gnuplot" ,gnuplot)
          ("libxls" ,libxls)
          ("libxlsxwriter" ,libxlsxwriter)
          ("libxml2" ,libxml2)
          ("libzip" ,libzip)
          ("ncurses" ,ncurses)))
      (native-inputs
        `(("pkg-config" ,pkg-config)
          ("which" ,which)
          ("bison" ,bison)))
      (synopsis "Spreadsheet program with vim-like keybindings")
      (description
 "@code{sc-im} is a highly configurable spreadsheet program
 providing a vim-like experience.  @code{sc-im} supports @{gnuplot} interaction,
 functions for sorting and filtering, 256 color support, and much more.")
      (license bsd-4)))
