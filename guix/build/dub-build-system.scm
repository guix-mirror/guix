;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (guix build dub-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build syscalls)
  #:use-module (guix build utils)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            dub-build))

;; Commentary:
;;
;; Builder-side code of the DUB (the build tool for D) build system.
;;
;; Code:

;; FIXME: Needs to be parsed from url not package name.
(define (package-name->d-package-name name)
  "Return the package name of NAME."
  (match (string-split name #\-)
    (("d" rest ...)
     (string-join rest "-"))
    (_ #f)))

(define* (configure #:key inputs #:allow-other-keys)
  "Prepare one new directory with all the required dependencies.
   It's necessary to do this (instead of just using /gnu/store as the
   directory) because we want to hide the libraries in subdirectories
   lib/dub/... instead of polluting the user's profile root."
  (let* ((dir (mkdtemp! "/tmp/dub.XXXXXX"))
         (vendor-dir (string-append dir "/vendor")))
    (setenv "HOME" dir)
    (mkdir vendor-dir)
    (for-each
      (match-lambda
        ((name . path)
         (let* ((d-package (package-name->d-package-name name))
                (d-basename (basename path)))
           (when (and d-package path)
             (match (string-split (basename path) #\-)
               ((_ ... version)
                (symlink (string-append path "/lib/dub/" d-basename)
                         (string-append vendor-dir "/" d-basename))))))))
      inputs)
    (invoke "dub" "add-path" vendor-dir)
    #t))

(define (grep string file-name)
  "Find the first occurrence of STRING in the file named FILE-NAME.
   Return the position of this occurrence, or #f if none was found."
  (string-contains (call-with-input-file file-name get-string-all)
                   string))

(define (grep* string file-name)
  "Find the first occurrence of STRING in the file named FILE-NAME.
   Return the position of this occurrence, or #f if none was found.
   If the file named FILE-NAME doesn't exist, return #f."
  (catch 'system-error
    (lambda ()
      (grep string file-name))
    (lambda args
      #f)))

(define* (build #:key (dub-build-flags '())
                #:allow-other-keys)
  "Build a given DUB package."
  (unless (or (grep* "sourceLibrary" "package.json")
              (grep* "sourceLibrary" "dub.sdl") ; note: format is different!
              (grep* "sourceLibrary" "dub.json"))
    (apply invoke `("dub" "build" ,@dub-build-flags))
    (substitute* ".dub/dub.json"
      (("\"lastUpgrade\": \"[^\"]*\"")
       "\"lastUpgrade\": \"1970-01-01T00:00:00.0000000\"")))
  #t)

(define* (check #:key tests? #:allow-other-keys)
  (when tests?
    (invoke "dub" "test")
    (substitute* ".dub/dub.json"
      (("\"lastUpgrade\": \"[^\"]*\"")
       "\"lastUpgrade\": \"1970-01-01T00:00:00.0000000\"")))
  #t)

(define* (install #:key inputs outputs #:allow-other-keys)
  "Install a given DUB package."
  (let* ((out (assoc-ref outputs "out"))
         (outbin (string-append out "/bin"))
         (outlib (string-append out "/lib/dub/" (basename out))))
    (mkdir-p outbin)
    ;; TODO remove "-test-application"
    (copy-recursively "bin" outbin)
    (mkdir-p outlib)
    (copy-recursively "." (string-append outlib))
    #t))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (replace 'configure configure)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)))

(define* (dub-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given DUB package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
