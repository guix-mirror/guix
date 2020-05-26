;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix quirks)
  #:use-module ((guix build utils) #:select (substitute*))
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 rdelim)
  #:export (%quirks

            patch?
            applicable-patch?
            apply-patch

            %patches))

;;; Commentary:
;;;
;;; Time traveling is a challenge!  Sometimes, going back to the past requires
;;; adjusting the old source code so it can be evaluated with our modern day
;;; Guile and against our modern Guix APIs.  This file describes quirks found
;;; in old Guix revisions, along with ways to address them or patch them.
;;;
;;; Code:

(define (syscalls-reexports-local-variables? source)
  "Return true if (guix build syscalls) contains the bug described at
<https://bugs.gnu.org/36723>."
  (catch 'system-error
    (lambda ()
      (define content
        (call-with-input-file (string-append source
                                             "/guix/build/syscalls.scm")
          read-string))

      ;; The faulty code would use the 're-export' macro, causing the
      ;; 'AT_SYMLINK_NOFOLLOW' local variable to be re-exported when using
      ;; Guile > 2.2.4.
      (string-contains content "(re-export variable)"))
    (lambda args
      (if (= ENOENT (system-error-errno args))
          #f
          (apply throw args)))))

(define (guile-2.2.4)
  (module-ref (resolve-interface '(gnu packages guile))
              'guile-2.2.4))

(define %quirks
  ;; List of predicate/package pairs.  This allows us to provide information
  ;; about specific Guile versions that old Guix revisions might need to use
  ;; just to be able to build and run the trampoline in %SELF-BUILD-FILE.  See
  ;; <https://bugs.gnu.org/37506>
  `((,syscalls-reexports-local-variables? . ,guile-2.2.4)))


;;;
;;; Patches.
;;;

;; Patch to apply to a source tree.
(define-record-type <patch>
  (patch predicate application)
  patch?
  (predicate    patch-predicate)                  ;procedure
  (application  patch-application))               ;procedure

(define (applicable-patch? patch source commit)
  "Return true if PATCH is applicable to SOURCE, a directory, which
corresponds to the given Guix COMMIT, a SHA1 hexadecimal string."
  ;; The predicate is passed COMMIT so that it can choose to only apply to
  ;; ancestors.
  ((patch-predicate patch) source commit))

(define (apply-patch patch source)
  "Apply PATCH onto SOURCE, directly modifying files beneath it."
  ((patch-application patch) source))

(define %self-build-file
  ;; The file containing code to build Guix.
  "build-aux/build-self.scm")

(define %bug-41028-patch
  ;; Patch for <https://bugs.gnu.org/41028>.  The faulty code is the
  ;; 'compute-guix-derivation' body, which uses 'call-with-new-thread' without
  ;; importing (ice-9 threads).  However, the 'call-with-new-thread' binding
  ;; is no longer available in the default name space on Guile 3.0.
  (let ()
    (define (missing-ice-9-threads-import? source commit)
      ;; Return true if %SELF-BUILD-FILE is missing an (ice-9 threads) import.
      (define content
        (call-with-input-file (string-append source "/" %self-build-file)
          read-string))

      (and (string-contains content "(call-with-new-thread")
           (not (string-contains content "(ice-9 threads)"))))

    (define (add-missing-ice-9-threads-import source)
      ;; Add (ice-9 threads) import in the gexp of 'compute-guix-derivation'.
      (substitute* (string-append source "/" %self-build-file)
        (("^ +\\(use-modules \\(ice-9 match\\)\\)")
         (object->string '(use-modules (ice-9 match) (ice-9 threads))))))

   (patch missing-ice-9-threads-import? add-missing-ice-9-threads-import)))

(define %patches
  ;; Bits of past Guix revisions can become incompatible with newer Guix and
  ;; Guile.  This variable lists <patch> records for the Guix source tree that
  ;; apply to the Guix source.
  (list %bug-41028-patch))
