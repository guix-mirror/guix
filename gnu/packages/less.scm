;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages less)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages ncurses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public less
  (package
    (name "less")
    (version "551")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "mirror://gnu/less/less-"
                                 version ".tar.gz")
                  (string-append "http://www.greenwoodsoftware.com/less/less-"
                                 version ".tar.gz")))
       (patches (search-patches "less-hurd-path-max.patch"))
       (sha256
        (base32 "0ggyjl3yzn7c450zk1rixi9ls6asdhgqynhk34zsd0ckhmsm45pz"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)))
    (home-page "https://www.gnu.org/software/less/")
    (synopsis "Paginator for terminals")
    (description
     "GNU less is a pager, a program that allows you to view large amounts
of text in page-sized chunks.  Unlike traditional pagers, it allows both
backwards and forwards movement through the document.  It also does not have
to read the entire input file before starting, so it starts faster than most
text editors.")
    (license gpl3+))) ; some files are under GPLv2+
