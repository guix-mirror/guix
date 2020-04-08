;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu installer proxy)
  #:use-module (gnu services herd)
  #:export (set-http-proxy
            clear-http-proxy))

(define-syntax-rule (with-silent-shepherd exp ...)
  (parameterize ((shepherd-message-port
                  (%make-void-port "w")))
    exp ...))

(define (set-http-proxy proxy)
  (with-silent-shepherd
    (with-shepherd-action 'guix-daemon
        ('set-http-proxy proxy)
        result
      result)))

(define (clear-http-proxy)
  (with-silent-shepherd
    (with-shepherd-action 'guix-daemon
        ('set-http-proxy)
        result
      result)))

;; Local Variables:
;; eval: (put 'with-silent-shepherd 'scheme-indent-function 0)
;; End:
