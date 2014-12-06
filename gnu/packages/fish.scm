;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
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

(define-module (gnu packages fish)
  #:use-module (guix licenses)
  #:use-module (gnu packages doxygen)
  #:use-module (gnu packages ncurses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public fish
  (package
    (name "fish")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://fishshell.com/files/"
                                  version "/fish-" version ".tar.gz"))
              (sha256
               (base32
                "096rhi911s3j618cvp8fj9pb4jniy3y6415jvjg8bhszsp1x7r5p"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)))
    (inputs
     `(("ncurses" ,ncurses)))
    (arguments
     '(#:tests? #f)) ; no check target
    (synopsis "The friendly interactive shell")
    (description
     "Fish (friendly interactive shell) is a shell focused on interactive use,
discoverability, and friendliness.  Fish has very user-friendly and powerful
tab-completion, including descriptions of every completion, completion of
strings with wildcards, and many completions for specific commands.  It also
has extensive and discoverable help.  A special help command gives access to
all the fish documentation in your web browser.  Other features include smart
terminal handling based on terminfo, an easy to search history, and syntax
highlighting.")
    (home-page "http://fishshell.com/")
    (license gpl2)))
