;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
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

(define-module (gnu packages tmux)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages ncurses))

(define-public tmux
  (package
    (name "tmux")
    (version "1.7")
    (source (origin
             (method url-fetch)
             (uri (string-append
                    "mirror://sourceforge/tmux/tmux/tmux-"
                    version "/tmux-" version ".tar.gz"))
             (sha256
              (base32
               "0ywy1x2g905hmhkdz418ik42lcvnhnwr8fv63rcqczfg27d6nd38"))))
    (build-system gnu-build-system)
    (inputs
     `(("libevent" ,libevent)
       ("ncurses" ,ncurses)))
    (home-page "http://tmux.sourceforge.net/")
    (synopsis "Terminal multiplexer")
    (description
     "tmux is a terminal multiplexer: it enables a number of terminals (or
windows), each running a separate program, to be created, accessed, and
controlled from a single screen. tmux may be detached from a screen and
continue running in the background, then later reattached.")
    (license isc)))
