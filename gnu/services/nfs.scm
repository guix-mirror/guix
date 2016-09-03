;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
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

(define-module (gnu services nfs)
  #:use-module (gnu)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages onc-rpc)
  #:use-module (guix)
  #:use-module (guix records)
  #:export (rpcbind-service-type
            rpcbind-configuration
            rpcbind-configuration?))

(define-record-type* <rpcbind-configuration>
  rpcbind-configuration make-rpcbind-configuration
  rpcbind-configuration?
  (rpcbind             rpcbind-configuration-rpcbind
                       (default rpcbind))
  (warm-start?         rpcbind-configuration-warm-start?
                       (default #t)))

(define rpcbind-service-type
  (shepherd-service-type
   'rpcbind
   (lambda (config)
     (define pkg
       (rpcbind-configuration-rpcbind config))

     (define rpcbind-command
       #~(list (string-append #$pkg "/bin/rpcbind") "-f"
               #$@(if (rpcbind-configuration-warm-start? config) '("-w") '())))

     (shepherd-service
      (documentation "Start the RPC bind daemon.")
      (requirement '(networking))
      (provision '(rpcbind-daemon))

      (start #~(make-forkexec-constructor #$rpcbind-command))
      (stop #~(make-kill-destructor))))))
