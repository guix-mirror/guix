;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (test-pack)
  #:use-module (guix scripts pack)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix profiles)
  #:use-module (guix monads)
  #:use-module (guix grafts)
  #:use-module (guix tests)
  #:use-module (guix gexp)
  #:use-module (gnu packages bootstrap)
  #:use-module (srfi srfi-64))

(define %store
  (open-connection-for-tests))

;; Globally disable grafts because they can trigger early builds.
(%graft? #f)

(define-syntax-rule (test-assertm name store exp)
  (test-assert name
    (run-with-store store exp
                    #:guile-for-build (%guile-for-build))))

(define %gzip-compressor
  ;; Compressor that uses the bootstrap 'gzip'.
  ((@ (guix scripts pack) compressor) "gzip"
   "gz"
   #~(#+(file-append %bootstrap-coreutils&co "/bin/gzip") "-6n")))

(define %tar-bootstrap %bootstrap-coreutils&co)


(test-begin "pack")

(unless (network-reachable?) (test-skip 1))
(test-assertm "self-contained-tarball" %store
  (mlet* %store-monad
      ((profile (profile-derivation (packages->manifest
                                     (list %bootstrap-guile))
                                    #:hooks '()
                                    #:locales? #f))
       (tarball (self-contained-tarball "pack" profile
                                        #:symlinks '(("/bin/Guile"
                                                      -> "bin/guile"))
                                        #:compressor %gzip-compressor
                                        #:archiver %tar-bootstrap))
       (check   (gexp->derivation
                 "check-tarball"
                 #~(let ((bin (string-append "." #$profile "/bin")))
                     (setenv "PATH"
                             (string-append #$%tar-bootstrap "/bin"))
                     (system* "tar" "xvf" #$tarball)
                     (mkdir #$output)
                     (exit
                      (and (file-exists? (string-append bin "/guile"))
                           (string=? (string-append #$%bootstrap-guile "/bin")
                                     (readlink bin))
                           (string=? (string-append ".." #$profile
                                                    "/bin/guile")
                                     (readlink "bin/Guile"))))))))
    (built-derivations (list check))))

(test-end)

;; Local Variables:
;; eval: (put 'test-assertm 'scheme-indent-function 2)
;; End:
