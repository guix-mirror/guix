;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Alex Kost <alezost@gmail.com>
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

(define-module (test-profiles)
  #:use-module (guix tests)
  #:use-module (guix profiles)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix grafts)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bootstrap)
  #:use-module ((gnu packages base) #:prefix packages:)
  #:use-module ((gnu packages guile) #:prefix packages:)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

;; Test the (guix profiles) module.

(define %store
  (open-connection-for-tests))

;; Globally disable grafts because they can trigger early builds.
(%graft? #f)

;; Example manifest entries.

(define guile-1.8.8
  (manifest-entry
    (name "guile")
    (version "1.8.8")
    (item "/gnu/store/...")
    (output "out")))

(define guile-2.0.9
  (manifest-entry
    (name "guile")
    (version "2.0.9")
    (item "/gnu/store/...")
    (output "out")))

(define guile-2.0.9:debug
  (manifest-entry (inherit guile-2.0.9)
    (output "debug")))

(define glibc
  (manifest-entry
    (name "glibc")
    (version "2.19")
    (item "/gnu/store/...")
    (output "out")))


(test-begin "profiles")

(test-assert "manifest-installed?"
  (let ((m (manifest (list guile-2.0.9 guile-2.0.9:debug))))
    (and (manifest-installed? m (manifest-pattern (name "guile")))
         (manifest-installed? m (manifest-pattern
                                  (name "guile") (output "debug")))
         (manifest-installed? m (manifest-pattern
                                  (name "guile") (output "out")
                                  (version "2.0.9")))
         (not (manifest-installed?
               m (manifest-pattern (name "guile") (version "1.8.8"))))
         (not (manifest-installed?
               m (manifest-pattern (name "guile") (output "foobar")))))))

(test-assert "manifest-matching-entries"
  (let* ((e (list guile-2.0.9 guile-2.0.9:debug))
         (m (manifest e)))
    (and (equal? e
                 (manifest-matching-entries m
                                            (list (manifest-pattern
                                                    (name "guile")
                                                    (output #f)))))
         (equal? (list guile-2.0.9)
                 (manifest-matching-entries m
                                            (list (manifest-pattern
                                                    (name "guile")
                                                    (version "2.0.9"))))))))

(test-assert "manifest-matching-entries, no match"
  (let ((m (manifest (list guile-2.0.9)))
        (p (manifest-pattern (name "python"))))
    (guard (c ((unmatched-pattern-error? c)
               (and (eq? p (unmatched-pattern-error-pattern c))
                    (eq? m (unmatched-pattern-error-manifest c)))))
      (manifest-matching-entries m (list p))
      #f)))

(test-equal "concatenate-manifests"
  (manifest (list guile-2.0.9 glibc))
  (concatenate-manifests (list (manifest (list guile-2.0.9))
                               (manifest (list glibc)))))

(test-assert "manifest-remove"
  (let* ((m0 (manifest (list guile-2.0.9 guile-2.0.9:debug)))
         (m1 (manifest-remove m0
                              (list (manifest-pattern (name "guile")))))
         (m2 (manifest-remove m1
                              (list (manifest-pattern (name "guile"))))) ; same
         (m3 (manifest-remove m2
                              (list (manifest-pattern
                                      (name "guile") (output "debug")))))
         (m4 (manifest-remove m3
                              (list (manifest-pattern (name "guile"))))))
    (match (manifest-entries m2)
      ((($ <manifest-entry> "guile" "2.0.9" "debug"))
       (and (equal? m1 m2)
            (null? (manifest-entries m3))
            (null? (manifest-entries m4)))))))

(test-assert "manifest-add"
  (let* ((m0 (manifest '()))
         (m1 (manifest-add m0 (list guile-1.8.8)))
         (m2 (manifest-add m1 (list guile-2.0.9)))
         (m3 (manifest-add m2 (list guile-2.0.9:debug)))
         (m4 (manifest-add m3 (list guile-2.0.9:debug))))
    (and (match (manifest-entries m1)
           ((($ <manifest-entry> "guile" "1.8.8" "out")) #t)
           (_ #f))
         (match (manifest-entries m2)
           ((($ <manifest-entry> "guile" "2.0.9" "out")) #t)
           (_ #f))
         (equal? m3 m4))))

(test-equal "manifest-add removes duplicates"    ;<https://bugs.gnu.org/30569>
  (list guile-2.0.9)
  (manifest-entries (manifest-add (manifest '())
                                  (list guile-2.0.9 guile-2.0.9))))

(test-equal "manifest->code, simple"
  '(begin
     (specifications->manifest (list "guile" "guile:debug" "glibc")))
  (manifest->code (manifest (list guile-2.0.9 guile-2.0.9:debug glibc))))

(test-equal "manifest->code, simple, versions"
  '(begin
     (specifications->manifest (list "guile@2.0.9" "guile@2.0.9:debug"
                                     "glibc@2.19")))
  (manifest->code (manifest (list guile-2.0.9 guile-2.0.9:debug glibc))
                  #:entry-package-version manifest-entry-version))

(test-equal "manifest->code, transformations"
  '(begin
     (use-modules (guix transformations))

     (define transform1
       (options->transformation '((foo . "bar"))))

     (packages->manifest
      (list (transform1 (specification->package "guile"))
            (specification->package "glibc"))))
  (manifest->code (manifest (list (manifest-entry
                                    (inherit guile-2.0.9)
                                    (properties `((transformations
                                                   . ((foo . "bar"))))))
                                  glibc))))

(test-assert "manifest-perform-transaction"
  (let* ((m0 (manifest (list guile-2.0.9 guile-2.0.9:debug)))
         (t1 (manifest-transaction
              (install (list guile-1.8.8))
              (remove (list (manifest-pattern (name "guile")
                                              (output "debug"))))))
         (t2 (manifest-transaction
              (remove (list (manifest-pattern (name "guile")
                                              (version "2.0.9")
                                              (output #f))))))
         (m1 (manifest-perform-transaction m0 t1))
         (m2 (manifest-perform-transaction m1 t2))
         (m3 (manifest-perform-transaction m0 t2)))
    (and (match (manifest-entries m1)
           ((($ <manifest-entry> "guile" "1.8.8" "out")) #t)
           (_ #f))
         (equal? m1 m2)
         (null? (manifest-entries m3)))))

(test-assert "manifest-transaction-effects"
  (let* ((m0 (manifest (list guile-1.8.8)))
         (t  (manifest-transaction
              (install (list guile-2.0.9 glibc)))))
    (let-values (((remove install upgrade downgrade)
                  (manifest-transaction-effects m0 t)))
      (and (null? remove) (null? downgrade)
           (equal? (list glibc) install)
           (equal? (list (cons guile-1.8.8 guile-2.0.9)) upgrade)))))

(test-assert "manifest-transaction-effects no double install or upgrades"
  (let* ((m0 (manifest (list guile-1.8.8)))
         (t  (manifest-transaction
              (install (list guile-2.0.9 glibc glibc)))))
    (let-values (((remove install upgrade downgrade)
                  (manifest-transaction-effects m0 t)))
      (and (null? remove) (null? downgrade)
           (equal? (list glibc) install)
           (equal? (list (cons guile-1.8.8 guile-2.0.9)) upgrade)))))

(test-assert "manifest-transaction-effects and downgrades"
  (let* ((m0 (manifest (list guile-2.0.9)))
         (t  (manifest-transaction (install (list guile-1.8.8)))))
    (let-values (((remove install upgrade downgrade)
                  (manifest-transaction-effects m0 t)))
      (and (null? remove) (null? install) (null? upgrade)
           (equal? (list (cons guile-2.0.9 guile-1.8.8)) downgrade)))))

(test-assert "manifest-transaction-effects no double downgrade"
  (let* ((m0 (manifest (list guile-2.0.9)))
         (t  (manifest-transaction (install (list guile-1.8.8 guile-1.8.8)))))
    (let-values (((remove install upgrade downgrade)
                  (manifest-transaction-effects m0 t)))
      (and (null? remove) (null? install) (null? upgrade)
           (equal? (list (cons guile-2.0.9 guile-1.8.8)) downgrade)))))

(test-assert "manifest-transaction-effects and pseudo-upgrades"
  (let* ((m0 (manifest (list guile-2.0.9)))
         (t  (manifest-transaction (install (list guile-2.0.9)))))
    (let-values (((remove install upgrade downgrade)
                  (manifest-transaction-effects m0 t)))
      (and (null? remove) (null? install) (null? downgrade)
           (equal? (list (cons guile-2.0.9 guile-2.0.9)) upgrade)))))

(test-assert "manifest-transaction-null?"
  (manifest-transaction-null? (manifest-transaction)))

(test-assert "manifest-transaction-removal-candidate?"
  (let ((m (manifest (list guile-2.0.9)))
        (t (manifest-transaction
            (remove (list (manifest-pattern (name "guile")))))))
    (and (manifest-transaction-removal-candidate? guile-2.0.9 t)
         (not (manifest-transaction-removal-candidate? glibc t)))))

(test-assert "manifest-transaction-effects no double removal"
  (let* ((m0 (manifest (list guile-2.0.9)))
         (t  (manifest-transaction
              (remove (list (manifest-pattern (name "guile")))))))
    (let-values (((remove install upgrade downgrade)
                  (manifest-transaction-effects m0 t)))
      (and (= 1 (length remove))
           (manifest-transaction-removal-candidate? guile-2.0.9 t)
           (null? install) (null? downgrade) (null? upgrade)))))

(test-assert "package->development-manifest"
  (let ((manifest (package->development-manifest packages:hello)))
    (every (lambda (name)
             (manifest-installed? manifest
                                  (manifest-pattern (name name))))
           '("gcc" "binutils" "glibc" "coreutils" "grep" "sed"))))

(test-assertm "profile-derivation"
  (mlet* %store-monad
      ((entry ->   (package->manifest-entry %bootstrap-guile))
       (guile      (package->derivation %bootstrap-guile))
       (drv        (profile-derivation (manifest (list entry))
                                       #:hooks '()
                                       #:locales? #f))
       (profile -> (derivation->output-path drv))
       (bindir ->  (string-append profile "/bin"))
       (_          (built-derivations (list drv))))
    (return (and (file-exists? (string-append bindir "/guile"))
                 (string=? (dirname (readlink bindir))
                           (derivation->output-path guile))))))

(test-assertm "profile-derivation, ordering & collisions"
  ;; ENTRY1 and ENTRY2 both provide 'bin/guile'--a collision.  Make sure
  ;; ENTRY1 "wins" over ENTRY2.  See <https://bugs.gnu.org/49102>.
  (mlet* %store-monad
      ((entry1 ->  (package->manifest-entry %bootstrap-guile))
       (entry2 ->  (manifest-entry
                     (name "fake-guile")
                     (version "0")
                     (item (computed-file
                            "fake-guile"
                            #~(begin
                                (mkdir #$output)
                                (mkdir (string-append #$output "/bin"))
                                (call-with-output-file
                                    (string-append #$output "/bin/guile")
                                  (lambda (port)
                                    (display "Fake!\n" port))))))))
       (guile      (package->derivation %bootstrap-guile))
       (drv        (profile-derivation (manifest (list entry1 entry2))
                                       #:hooks '()
                                       #:locales? #f))
       (profile -> (derivation->output-path drv))
       (bindir ->  (string-append profile "/bin"))
       (file ->    (string-append bindir "/guile"))
       (_          (built-derivations (list drv))))
    (return (string=? (readlink file)
                      (string-append
                       (derivation->output-path guile)
                       "/bin/guile")))))

(test-assertm "load-profile"
  (mlet* %store-monad
      ((entry ->   (package->manifest-entry %bootstrap-guile))
       (guile      (package->derivation %bootstrap-guile))
       (drv        (profile-derivation (manifest (list entry))
                                       #:hooks '()
                                       #:locales? #f))
       (profile -> (derivation->output-path drv))
       (bindir ->  (string-append profile "/bin"))
       (_          (built-derivations (list drv))))
    (define-syntax-rule (with-environment-excursion exp ...)
      (let ((env (environ)))
        (dynamic-wind
          (const #t)
          (lambda () exp ...)
          (lambda () (environ env)))))

    (return (and (with-environment-excursion
                  (load-profile profile)
                  (and (string-prefix? (string-append bindir ":")
                                       (getenv "PATH"))
                       (getenv "GUILE_LOAD_PATH")))
                 (with-environment-excursion
                  (load-profile profile #:pure? #t #:white-list '())
                  (equal? (list (string-append "PATH=" bindir))
                          (environ)))))))

(test-assertm "<profile>"
  (mlet* %store-monad
      ((entry ->   (package->manifest-entry %bootstrap-guile))
       (profile -> (profile (hooks '()) (locales? #f)
                            (content (manifest (list entry)))))
       (drv        (lower-object profile))
       (profile -> (derivation->output-path drv))
       (bindir ->  (string-append profile "/bin"))
       (_          (built-derivations (list drv))))
    (return (file-exists? (string-append bindir "/guile")))))

(test-assertm "profile-derivation relative symlinks, one entry"
  (mlet* %store-monad
      ((entry ->   (package->manifest-entry %bootstrap-guile))
       (guile      (package->derivation %bootstrap-guile))
       (drv        (profile-derivation (manifest (list entry))
                                       #:relative-symlinks? #t
                                       #:hooks '()
                                       #:locales? #f))
       (profile -> (derivation->output-path drv))
       (bindir ->  (string-append profile "/bin"))
       (_          (built-derivations (list drv))))
    (return (and (file-exists? (string-append bindir "/guile"))
                 (string=? (readlink bindir)
                           (string-append "../"
                                          (basename
                                           (derivation->output-path guile))
                                          "/bin"))))))

(unless (network-reachable?) (test-skip 1))
(test-assertm "profile-derivation relative symlinks, two entries"
  (mlet* %store-monad
      ((manifest -> (packages->manifest
                     (list %bootstrap-guile gnu-make-for-tests)))
       (guile       (package->derivation %bootstrap-guile))
       (make        (package->derivation gnu-make-for-tests))
       (drv         (profile-derivation manifest
                                        #:relative-symlinks? #t
                                        #:hooks '()
                                        #:locales? #f))
       (profile ->  (derivation->output-path drv))
       (bindir ->   (string-append profile "/bin"))
       (_           (built-derivations (list drv))))
    (return (and (file-exists? (string-append bindir "/guile"))
                 (file-exists? (string-append bindir "/make"))
                 (string=? (readlink (string-append bindir "/guile"))
                           (string-append "../../"
                                          (basename
                                           (derivation->output-path guile))
                                          "/bin/guile"))
                 (string=? (readlink (string-append bindir "/make"))
                           (string-append "../../"
                                          (basename
                                           (derivation->output-path make))
                                          "/bin/make"))))))

(test-assertm "profile-derivation, inputs"
  (mlet* %store-monad
      ((entry ->   (package->manifest-entry packages:glibc "debug"))
       (drv        (profile-derivation (manifest (list entry))
                                       #:hooks '()
                                       #:locales? #f)))
    (return (derivation-inputs drv))))

(test-assertm "profile-derivation, cross-compilation"
  (mlet* %store-monad
      ((manifest -> (packages->manifest (list packages:sed packages:grep)))
       (target ->   "arm-linux-gnueabihf")
       (grep        (package->cross-derivation packages:grep target))
       (sed         (package->cross-derivation packages:sed target))
       (locales     (package->derivation packages:glibc-utf8-locales))
       (drv         (profile-derivation manifest
                                        #:hooks '()
                                        #:locales? #t
                                        #:target target)))
    (define (find-input package)
      (let ((name (string-append (package-full-name package "-") ".drv")))
        (any (lambda (input)
               (let ((input (derivation-input-path input)))
                 (and (string-suffix? name input) input)))
             (derivation-inputs drv))))

    ;; The inputs for grep and sed should be cross-build derivations, but that
    ;; for the glibc-utf8-locales should be a native build.
    (return (and (string=? (derivation-system drv) (%current-system))
                 (string=? (find-input packages:grep)
                           (derivation-file-name grep))
                 (string=? (find-input packages:sed)
                           (derivation-file-name sed))
                 (string=? (find-input packages:glibc-utf8-locales)
                           (derivation-file-name locales))))))

(test-assert "package->manifest-entry defaults to \"out\""
  (let ((outputs (package-outputs packages:glibc)))
    (equal? (manifest-entry-output
             (package->manifest-entry (package
                                        (inherit packages:glibc)
                                        (outputs (reverse outputs)))))
            (manifest-entry-output
             (package->manifest-entry packages:glibc))
            "out")))

(test-assertm "profile-manifest, search-paths"
  (mlet* %store-monad
      ((guile ->   (package
                     (inherit %bootstrap-guile)
                     (native-search-paths
                      (package-native-search-paths packages:guile-2.0))))
       (entry ->   (package->manifest-entry guile))
       (drv        (profile-derivation (manifest (list entry))
                                       #:hooks '()
                                       #:locales? #f))
       (profile -> (derivation->output-path drv)))
    (mbegin %store-monad
      (built-derivations (list drv))

      ;; Read the manifest back and make sure search paths are preserved.
      (let ((manifest (profile-manifest profile)))
        (match (manifest-entries manifest)
          ((result)
           (return (equal? (manifest-entry-search-paths result)
                           (manifest-entry-search-paths entry)
                           (package-native-search-paths
                            packages:guile-2.0)))))))))

(test-assert "package->manifest-entry, search paths"
  ;; See <http://bugs.gnu.org/22073>.
  (let ((mpl (@ (gnu packages python-xyz) python2-matplotlib)))
    (lset= eq?
           (package-transitive-native-search-paths mpl)
           (manifest-entry-search-paths
            (package->manifest-entry mpl)))))

(test-assert "packages->manifest, no duplicates"
  (let ((expected
         (manifest
          (list
           (package->manifest-entry packages:guile-2.2))))
        (manifest (packages->manifest
                   (list packages:guile-2.2 packages:guile-2.2))))
    (every manifest-entry=? (manifest-entries expected)
           (manifest-entries manifest))))

(test-equal "packages->manifest, propagated inputs"
  (map (match-lambda
         ((label package)
          (list (package-name package) (package-version package)
                package)))
       (package-propagated-inputs packages:guile-2.2))
  (map (lambda (entry)
         (list (manifest-entry-name entry)
               (manifest-entry-version entry)
               (manifest-entry-item entry)))
       (manifest-entry-dependencies
        (package->manifest-entry packages:guile-2.2))))

(test-assert "manifest-entry-parent"
  (let ((entry (package->manifest-entry packages:guile-2.2)))
    (match (manifest-entry-dependencies entry)
      ((dependencies ..1)
       (and (every (lambda (parent)
                     (eq? entry (force parent)))
                   (map manifest-entry-parent dependencies))
            (not (force (manifest-entry-parent entry))))))))

(test-assertm "read-manifest"
  (mlet* %store-monad ((manifest -> (packages->manifest
                                     (list (package
                                             (inherit %bootstrap-guile)
                                             (native-search-paths
                                              (package-native-search-paths
                                               packages:guile-2.0))))))
                       (drv (profile-derivation manifest
                                                #:hooks '()
                                                #:locales? #f))
                       (out -> (derivation->output-path drv)))
    (define (entry->sexp entry)
      (list (manifest-entry-name entry)
            (manifest-entry-version entry)
            (manifest-entry-search-paths entry)
            (manifest-entry-dependencies entry)
            (force (manifest-entry-parent entry))))

    (mbegin %store-monad
      (built-derivations (list drv))
      (let ((manifest2 (profile-manifest out)))
        (return (equal? (map entry->sexp (manifest-entries manifest))
                        (map entry->sexp (manifest-entries manifest2))))))))

(test-equal "collision"
  '(("guile-bootstrap" "2.0") ("guile-bootstrap" "42"))
  (guard (c ((profile-collision-error? c)
             (let ((entry1 (profile-collision-error-entry c))
                   (entry2 (profile-collision-error-conflict c)))
               (list (list (manifest-entry-name entry1)
                           (manifest-entry-version entry1))
                     (list (manifest-entry-name entry2)
                           (manifest-entry-version entry2))))))
    (run-with-store %store
      (mlet* %store-monad ((p0 -> (package
                                    (inherit %bootstrap-guile)
                                    (version "42")))
                           (p1 -> (dummy-package "p1"
                                    (propagated-inputs `(("p0" ,p0)))))
                           (manifest -> (packages->manifest
                                         (list %bootstrap-guile p1)))
                           (drv (profile-derivation manifest
                                                    #:hooks '()
                                                    #:locales? #f)))
        (return #f)))))

(test-equal "collision of propagated inputs"
  '(("guile-bootstrap" "2.0") ("guile-bootstrap" "42"))
  (guard (c ((profile-collision-error? c)
             (let ((entry1 (profile-collision-error-entry c))
                   (entry2 (profile-collision-error-conflict c)))
               (list (list (manifest-entry-name entry1)
                           (manifest-entry-version entry1))
                     (list (manifest-entry-name entry2)
                           (manifest-entry-version entry2))))))
    (run-with-store %store
      (mlet* %store-monad ((p0 -> (package
                                    (inherit %bootstrap-guile)
                                    (version "42")))
                           (p1 -> (dummy-package "p1"
                                    (propagated-inputs
                                     `(("guile" ,%bootstrap-guile)))))
                           (p2 -> (dummy-package "p2"
                                    (propagated-inputs
                                     `(("guile" ,p0)))))
                           (manifest -> (packages->manifest (list p1 p2)))
                           (drv (profile-derivation manifest
                                                    #:hooks '()
                                                    #:locales? #f)))
        (return #f)))))

(test-assertm "no collision"
  ;; Here we have an entry that is "lowered" (its 'item' field is a store file
  ;; name) and another entry (its 'item' field is a package) that is
  ;; equivalent.
  (mlet* %store-monad ((p -> (dummy-package "p"
                               (propagated-inputs
                                `(("guile" ,%bootstrap-guile)))))
                       (guile    (package->derivation %bootstrap-guile))
                       (entry -> (manifest-entry
                                   (inherit (package->manifest-entry
                                             %bootstrap-guile))
                                   (item (derivation->output-path guile))))
                       (manifest -> (manifest
                                     (list entry
                                           (package->manifest-entry p))))
                       (drv (profile-derivation manifest)))
    (return (->bool drv))))

(test-assertm "etc/profile"
  ;; Make sure we get an 'etc/profile' file that at least defines $PATH.
  (mlet* %store-monad
      ((guile ->   (package
                     (inherit %bootstrap-guile)
                     (native-search-paths
                      (package-native-search-paths packages:guile-2.0))))
       (entry ->   (package->manifest-entry guile))
       (drv        (profile-derivation (manifest (list entry))
                                       #:hooks '()
                                       #:locales? #f))
       (profile -> (derivation->output-path drv)))
    (mbegin %store-monad
      (built-derivations (list drv))
      (let* ((pipe (open-input-pipe
                    (string-append "unset GUIX_PROFILE; "
                                   ;; 'source' is a Bashism; use '.' (dot).
                                   ". " profile "/etc/profile; "
                                   ;; Don't try to parse set(1) output because
                                   ;; it differs among shells; just use echo.
                                   "echo $PATH")))
             (path (get-string-all pipe)))
        (return
         (and (zero? (close-pipe pipe))
              (string-contains path (string-append profile "/bin"))))))))

(test-assertm "etc/profile when etc/ already exists"
  ;; Here 'union-build' makes the profile's etc/ a symlink to the package's
  ;; etc/ directory, which makes it read-only.  Make sure the profile build
  ;; handles that.
  (mlet* %store-monad
      ((thing ->   (dummy-package "dummy"
                     (build-system trivial-build-system)
                     (arguments
                      `(#:guile ,%bootstrap-guile
                        #:builder
                        (let ((out (assoc-ref %outputs "out")))
                          (mkdir out)
                          (mkdir (string-append out "/etc"))
                          (call-with-output-file (string-append out "/etc/foo")
                            (lambda (port)
                              (display "foo!" port)))
                          #t)))))
       (entry ->   (package->manifest-entry thing))
       (drv        (profile-derivation (manifest (list entry))
                                       #:hooks '()
                                       #:locales? #f))
       (profile -> (derivation->output-path drv)))
    (mbegin %store-monad
      (built-derivations (list drv))
      (return (and (file-exists? (string-append profile "/etc/profile"))
                   (string=? (call-with-input-file
                                 (string-append profile "/etc/foo")
                               get-string-all)
                             "foo!"))))))

(test-assertm "etc/profile when etc/ is a symlink"
  ;; When etc/ is a symlink, the unsymlink code in 0.8.2 would fail
  ;; gracelessly because 'scandir' would return #f.
  (mlet* %store-monad
      ((thing ->   (dummy-package "dummy"
                     (build-system trivial-build-system)
                     (arguments
                      `(#:guile ,%bootstrap-guile
                        #:builder
                        (let ((out (assoc-ref %outputs "out")))
                          (mkdir out)
                          (mkdir (string-append out "/foo"))
                          (symlink "foo" (string-append out "/etc"))
                          (call-with-output-file (string-append out "/etc/bar")
                            (lambda (port)
                              (display "foo!" port)))
                          #t)))))
       (entry ->   (package->manifest-entry thing))
       (drv        (profile-derivation (manifest (list entry))
                                       #:hooks '()
                                       #:locales? #f))
       (profile -> (derivation->output-path drv)))
    (mbegin %store-monad
      (built-derivations (list drv))
      (return (and (file-exists? (string-append profile "/etc/profile"))
                   (string=? (call-with-input-file
                                 (string-append profile "/etc/bar")
                               get-string-all)
                             "foo!"))))))

(test-assertm "profile-derivation when etc/ is a relative symlink"
  ;; See <https://bugs.gnu.org/32686>.
  (mlet* %store-monad
      ((etc        (gexp->derivation
                    "etc"
                    #~(begin
                        (mkdir #$output)
                        (call-with-output-file (string-append #$output "/foo")
                          (lambda (port)
                            (display "Heya!" port))))))
       (thing ->   (dummy-package "dummy"
                     (build-system trivial-build-system)
                     (inputs
                      `(("etc" ,etc)))
                     (arguments
                      `(#:guile ,%bootstrap-guile
                        #:builder
                        (let ((out (assoc-ref %outputs "out"))
                              (etc (assoc-ref %build-inputs "etc")))
                          (mkdir out)
                          (symlink etc (string-append out "/etc"))
                          #t)))))
       (entry ->   (package->manifest-entry thing))
       (drv        (profile-derivation (manifest (list entry))
                                       #:relative-symlinks? #t
                                       #:hooks '()
                                       #:locales? #f))
       (profile -> (derivation->output-path drv)))
    (mbegin %store-monad
      (built-derivations (list drv))
      (return (string=? (call-with-input-file
                            (string-append profile "/etc/foo")
                          get-string-all)
                        "Heya!")))))

(test-equalm "union vs. dangling symlink"        ;<https://bugs.gnu.org/26949>
  "does-not-exist"
  (mlet* %store-monad
      ((thing1 ->  (dummy-package "dummy"
                     (build-system trivial-build-system)
                     (arguments
                      `(#:guile ,%bootstrap-guile
                        #:builder
                        (let ((out (assoc-ref %outputs "out")))
                          (mkdir out)
                          (symlink "does-not-exist"
                                   (string-append out "/dangling"))
                          #t)))))
       (thing2 ->  (package (inherit thing1) (name "dummy2")))
       (drv        (profile-derivation (packages->manifest
                                        (list thing1 thing2))
                                       #:hooks '()
                                       #:locales? #f))
       (profile -> (derivation->output-path drv)))
    (mbegin %store-monad
      (built-derivations (list drv))
      (return (readlink (readlink (string-append profile "/dangling")))))))

(test-equalm "profile in profile"
  '("foo" "0")

  ;; Make sure we can build a profile that has another profile has one of its
  ;; entries.  The new profile's /manifest and /etc/profile must override the
  ;; other's.
  (mlet* %store-monad
      ((prof0 (profile-derivation
               (manifest
                (list (package->manifest-entry %bootstrap-guile)))
               #:hooks '()
               #:locales? #f))
       (prof1 (profile-derivation
               (manifest (list (manifest-entry
                                 (name "foo")
                                 (version "0")
                                 (item prof0))))
               #:hooks '()
               #:locales? #f)))
    (mbegin %store-monad
      (built-derivations (list prof1))
      (let ((out (derivation->output-path prof1)))
        (return (and (file-exists?
                      (string-append out "/bin/guile"))
                     (let ((manifest (profile-manifest out)))
                       (match (manifest-entries manifest)
                         ((entry)
                          (list (manifest-entry-name entry)
                                (manifest-entry-version entry)))))))))))

(test-end "profiles")

;;; Local Variables:
;;; eval: (put 'dummy-package 'scheme-indent-function 1)
;;; End:
