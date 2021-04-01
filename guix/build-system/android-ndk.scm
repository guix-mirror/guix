;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build-system android-ndk)
  #:use-module (guix search-paths)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (android-ndk-build-system))

(define %android-ndk-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build android-ndk-build-system)
    (guix build syscalls)
    ,@%gnu-build-system-modules))

(define* (android-ndk-build name inputs
                            #:key
                            source
                            (tests? #t)
                            (test-target #f)
                            (phases '%standard-phases)
                            (outputs '("out"))
                            (make-flags #~'())
                            (search-paths '())
                            (system (%current-system))
                            (guile #f)
                            (imported-modules %android-ndk-build-system-modules)
                            (modules '((guix build android-ndk-build-system)
                                       (guix build utils))))
  "Build SOURCE using Android NDK, and with INPUTS."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          (android-ndk-build #:name #$name
                             #:source #+source
                             #:system #$system
                             #:test-target #$test-target
                             #:tests? #$tests?
                             #:phases #$phases
                             #:bootstrap-scripts '() ;no autotools machinery
                             #:make-flags
                             (cons* "-f"
                                    #$(file-append (gexp-input-thing
                                                    (car (assoc-ref inputs
                                                                    "android-build")))
                                                   "/share/android/build/core/main.mk")
                                    #$make-flags)
                             #:outputs #$(outputs->gexp outputs)
                             #:search-paths '#$(sexp->gexp
                                                (map search-path-specification->sexp
                                                     search-paths))
                             #:inputs #$(input-tuples->gexp inputs)))))

  (mlet %store-monad  ((guile (package->derivation (or guile (default-guile))
                                                   system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:guile-for-build guile)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."

  (define private-keywords
    '(#:target #:inputs #:native-inputs #:outputs))

  (and (not target) ;; TODO: support cross-compilation
       (bag
         (name name)
         (system system)
         (target target)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs

                        ;; Keep the standard inputs of 'gnu-build-system'
                        ,@(standard-packages)))
         (build-inputs `(("android-build" ,(module-ref (resolve-interface '(gnu packages android)) 'android-make-stub))
                         ("android-googletest" ,(module-ref (resolve-interface '(gnu packages android)) 'android-googletest))
                         ,@native-inputs))
         (outputs outputs)
         (build android-ndk-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define android-ndk-build-system
  (build-system
    (name 'android-ndk)
    (description
     "Android NDK build system, to build Android NDK packages")
    (lower lower)))
