;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-syntax-rule (test-assertm name exp)
  (test-assert name
    (run-with-store %store exp
                    #:guile-for-build (%guile-for-build))))

(define %gzip-compressor
  ;; Compressor that uses the bootstrap 'gzip'.
  ((@ (guix scripts pack) compressor) "gzip"
   %bootstrap-coreutils&co "gz" '("gzip" "-6n")))

(define %tar-bootstrap %bootstrap-coreutils&co)


(test-begin "pack")

(test-assertm "self-contained-tarball"
  (mlet* %store-monad
      ((profile (profile-derivation (packages->manifest
                                     (list %bootstrap-guile))
                                    #:hooks '()
                                    #:locales? #f))
       (tarball (self-contained-tarball "pack" profile
                                        #:symlinks '(("/bin/Guile"
                                                      -> "bin/guile"))
                                        #:compressor %gzip-compressor
                                        #:tar %tar-bootstrap))
       (check   (gexp->derivation
                 "check-tarball"
                 #~(let ((guile (string-append "." #$profile "/bin")))
                     (setenv "PATH"
                             (string-append #$%tar-bootstrap "/bin"))
                     (system* "tar" "xvf" #$tarball)
                     (mkdir #$output)
                     (exit
                      (and (file-exists? (string-append guile "/guile"))
                           (string=? (string-append #$%bootstrap-guile "/bin")
                                     (readlink guile))
                           (string=? (string-append (string-drop guile 1)
                                                    "/guile")
                                     (readlink "bin/Guile"))))))))
    (built-derivations (list check))))

(test-end)
