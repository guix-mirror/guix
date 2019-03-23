;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix packages)
  #:use-module (guix monads)
  #:use-module (guix grafts)
  #:use-module (guix tests)
  #:use-module (guix gexp)
  #:use-module (gnu packages bootstrap)
  #:use-module ((gnu packages compression) #:select (squashfs-tools-next))
  #:use-module (srfi srfi-64))

(define %store
  (open-connection-for-tests))

;; Globally disable grafts because they can trigger early builds.
(%graft? #f)

(define-syntax-rule (test-assertm name store exp)
  (test-assert name
    (let ((guile (package-derivation store %bootstrap-guile)))
      (run-with-store store exp
                      #:guile-for-build guile))))

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
                 (with-imported-modules '((guix build utils))
                   #~(begin
                       (use-modules (guix build utils)
                                    (srfi srfi-1))

                       (define store
                         ;; The unpacked store.
                         (string-append "." (%store-directory) "/"))

                       (define (canonical? file)
                         ;; Return #t if FILE is read-only and its mtime is 1.
                         (let ((st (lstat file)))
                           (or (not (string-prefix? store file))
                               (eq? 'symlink (stat:type st))
                               (and (= 1 (stat:mtime st))
                                    (zero? (logand #o222
                                                   (stat:mode st)))))))

                       (define bin
                         (string-append "." #$profile "/bin"))

                       (setenv "PATH"
                               (string-append #$%tar-bootstrap "/bin"))
                       (system* "tar" "xvf" #$tarball)
                       (mkdir #$output)
                       (exit
                        (and (file-exists? (string-append bin "/guile"))
                             (file-exists? store)
                             (every canonical?
                                    (find-files "." (const #t)
                                                #:directories? #t))
                             (string=? (string-append #$%bootstrap-guile "/bin")
                                       (readlink bin))
                             (string=? (string-append ".." #$profile
                                                      "/bin/guile")
                                       (readlink "bin/Guile")))))))))
    (built-derivations (list check))))

;; The following test needs guile-sqlite3, libgcrypt, etc. as a consequence of
;; commit c45477d2a1a651485feede20fe0f3d15aec48b39 and related changes.  Thus,
;; run it on the user's store, if it's available, on the grounds that these
;; dependencies may be already there, or we can get substitutes or build them
;; quite inexpensively; see <https://bugs.gnu.org/32184>.

(with-external-store store
  (unless store (test-skip 1))
  (test-assertm "self-contained-tarball + localstatedir" store
    (mlet* %store-monad
        ((guile   (set-guile-for-build (default-guile)))
         (profile (profile-derivation (packages->manifest
                                       (list %bootstrap-guile))
                                      #:hooks '()
                                      #:locales? #f))
         (tarball (self-contained-tarball "tar-pack" profile
                                          #:localstatedir? #t))
         (check   (gexp->derivation
                   "check-tarball"
                   #~(let ((bin (string-append "." #$profile "/bin")))
                       (setenv "PATH"
                               (string-append #$%tar-bootstrap "/bin"))
                       (system* "tar" "xvf" #$tarball)
                       (mkdir #$output)
                       (exit
                        (and (file-exists? "var/guix/db/db.sqlite")
                             (string=? (string-append #$%bootstrap-guile "/bin")
                                       (readlink bin))))))))
      (built-derivations (list check))))

  (unless store (test-skip 1))
  (test-assertm "docker-image + localstatedir" store
    (mlet* %store-monad
        ((guile   (set-guile-for-build (default-guile)))
         (profile (profile-derivation (packages->manifest
                                       (list %bootstrap-guile))
                                      #:hooks '()
                                      #:locales? #f))
         (tarball (docker-image "docker-pack" profile
                                #:symlinks '(("/bin/Guile" -> "bin/guile"))
                                #:localstatedir? #t))
         (check   (gexp->derivation
                   "check-tarball"
                   (with-imported-modules '((guix build utils))
                     #~(begin
                         (use-modules (guix build utils)
                                      (ice-9 match))

                         (define bin
                           (string-append "." #$profile "/bin"))

                         (setenv "PATH" (string-append #$%tar-bootstrap "/bin"))
                         (mkdir "base")
                         (with-directory-excursion "base"
                           (invoke "tar" "xvf" #$tarball))

                         (match (find-files "base" "layer.tar")
                           ((layer)
                            (invoke "tar" "xvf" layer)))

                         (when
                          (and (file-exists? (string-append bin "/guile"))
                               (file-exists? "var/guix/db/db.sqlite")
                               (string=? (string-append #$%bootstrap-guile "/bin")
                                         (pk 'binlink (readlink bin)))
                               (string=? (string-append #$profile "/bin/guile")
                                         (pk 'guilelink (readlink "bin/Guile"))))
                          (mkdir #$output)))))))
      (built-derivations (list check))))

  (unless store (test-skip 1))
  (test-assertm "squashfs-image + localstatedir" store
    (mlet* %store-monad
        ((guile   (set-guile-for-build (default-guile)))
         (profile (profile-derivation (packages->manifest
                                       (list %bootstrap-guile))
                                      #:hooks '()
                                      #:locales? #f))
         (image   (squashfs-image "squashfs-pack" profile
                                  #:symlinks '(("/bin" -> "bin"))
                                  #:localstatedir? #t))
         (check   (gexp->derivation
                   "check-tarball"
                   (with-imported-modules '((guix build utils))
                     #~(begin
                         (use-modules (guix build utils)
                                      (ice-9 match))

                         (define bin
                           (string-append "." #$profile "/bin"))

                         (setenv "PATH"
                                 (string-append #$squashfs-tools-next "/bin"))
                         (invoke "unsquashfs" #$image)
                         (with-directory-excursion "squashfs-root"
                           (when (and (file-exists? (string-append bin
                                                                   "/guile"))
                                      (file-exists? "var/guix/db/db.sqlite")
                                      (string=? (string-append #$%bootstrap-guile "/bin")
                                                (pk 'binlink (readlink bin)))

                                      ;; This is a relative symlink target.
                                      (string=? (string-drop
                                                 (string-append #$profile "/bin")
                                                 1)
                                                (pk 'guilelink (readlink "bin"))))
                             (mkdir #$output))))))))
      (built-derivations (list check)))))

(test-end)

;; Local Variables:
;; eval: (put 'test-assertm 'scheme-indent-function 2)
;; End:
