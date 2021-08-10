;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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
(define-module (gnu packages minetest)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system minetest)
  #:use-module ((guix licenses) #:prefix license:))

(define-public (minetest-topic topic-id)
  "Return an URL (as a string) pointing to the forum topic with
numeric identifier TOPIC-ID on the official Minetest forums."
  (string-append "https://forum.minetest.net/viewtopic.php?t="
                 (number->string topic-id)))
