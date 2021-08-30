;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
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

(define-module (gnu platforms arm)
  #:use-module (gnu platform)
  #:use-module (gnu packages linux)
  #:use-module (guix records)
  #:export (armv7-linux
            aarch64-linux))

(define armv7-linux
  (platform
   (target "arm-linux-gnueabihf")
   (system "armhf-linux")
   (linux-architecture "arm")))

(define aarch64-linux
  (platform
   (target "aarch64-linux-gnu")
   (system "aarch64-linux")
   (linux-architecture "arm64")))
