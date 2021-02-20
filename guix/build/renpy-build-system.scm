;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Leo Prikler <leo.prikler@student.tugraz.at>
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

(define-module (guix build renpy-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module ((guix build python-build-system) #:prefix python:)
  #:use-module (guix build json)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            renpy-build))

(define* (build #:key game #:allow-other-keys)
  (for-each make-file-writable
            (find-files game (lambda (pred stat)
                               (eq? (stat:type stat) 'directory))))
  (invoke "renpy"
          "--json-dump" (string-append game "/renpy-build.json")
          game
          ;; should be "compile", but renpy wants to compile itself really
          ;; badly if we do
          "quit")
  #t)

(define* (install #:key outputs game (output "out") #:allow-other-keys)
  (let* ((out (assoc-ref outputs output))
         (json-dump (call-with-input-file (string-append game
                                                         "/renpy-build.json")
                      read-json))
         (build (assoc-ref json-dump "build"))
         (executable-name (assoc-ref build "executable_name"))
         (directory-name (assoc-ref build "directory_name")))
    (let ((launcher (string-append out "/bin/" executable-name))
          (data (string-append out "/share/renpy/" directory-name)))
      (mkdir-p (string-append out "/bin"))
      (copy-recursively game data)
      ;; We don't actually want the metadata to be dumped in the output
      ;; directory
      (delete-file (string-append data "/renpy-build.json"))
      (call-with-output-file launcher
        (lambda (port)
          (format port "#!~a~%~a ~s \"$@\""
                  (which "bash")
                  (which "renpy")
                  data)))
      (chmod launcher #o755)))
  #t)

(define* (install-desktop-file #:key outputs game (output "out")
                               #:allow-other-keys)
  (let* ((out (assoc-ref outputs output))
         (json-dump (call-with-input-file (string-append game
                                                         "/renpy-build.json")
                      read-json))
         (build (assoc-ref json-dump "build"))
         (directory-name (assoc-ref build "directory_name"))
         (executable-name (assoc-ref build "executable_name")))
    (make-desktop-entry-file
     (string-append out "/share/applications/" executable-name ".desktop")
     #:name (assoc-ref json-dump "name")
     #:generic-name (assoc-ref build "display_name")
     #:exec (format #f "~a ~s"
                    (which "renpy")
                    (string-append out "/share/renpy/" directory-name))
     #:categories '("Game" "Visual Novel")))
  #t)

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (add-after 'unpack 'enable-bytecode-determinism
      (assoc-ref python:%standard-phases 'enable-bytecode-determinism))
    (delete 'bootstrap)
    (delete 'configure)
    (replace 'build build)
    (delete 'check)
    (replace 'install install)
    (add-after 'install 'install-desktop-file install-desktop-file)))

(define* (renpy-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  "Build the given Ren'py package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
