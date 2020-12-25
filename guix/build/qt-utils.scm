;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2019, 2020, 2021 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (guix build qt-utils)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (wrap-qt-program
            wrap-all-qt-programs))


(define (variables-for-wrapping base-directories)

  (define (collect-sub-dirs base-directories subdirectory)
    (filter-map
     (lambda (dir)
       (let ((directory (string-append dir subdirectory)))
         (if (directory-exists? directory) directory #f)))
     base-directories))

  (filter
   (lambda (var-to-wrap) (not (null? (last var-to-wrap))))
   (map
    (lambda (var-spec)
      `(,(first var-spec) = ,(collect-sub-dirs base-directories (last var-spec))))
    (list
     ;; these shall match the search-path-specification for Qt and KDE
     ;; libraries
     '("XDG_DATA_DIRS" "/share")
     '("XDG_CONFIG_DIRS" "/etc/xdg")
     '("QT_PLUGIN_PATH" "/lib/qt5/plugins")
     '("QML2_IMPORT_PATH" "/lib/qt5/qml")))))


(define* (wrap-qt-program* program #:key inputs output-dir)

  (define input-directories
    ;; FIXME: Filter out unwanted inputs, e.g. cmake
    (match inputs
           (((_ . dir) ...)
            dir)))

  (let ((vars-to-wrap (variables-for-wrapping
                       (cons output-dir input-directories))))
    (when (not (null? vars-to-wrap))
      (apply wrap-program program vars-to-wrap))))


(define* (wrap-qt-program program-name #:key inputs output)
  "Wrap the specified programm (which must reside in the OUTPUT's \"/bin\"
directory) with suitably set environment variables.

This is like qt-build-systems's phase \"qt-wrap\", but only the named program
is wrapped."
  (wrap-qt-program* (string-append output "/bin/" program-name)
                    #:output-dir output #:inputs inputs))


(define* (wrap-all-qt-programs #:key inputs outputs
                               (qt-wrap-excluded-outputs '())
                               #:allow-other-keys)
  "Implement qt-build-systems's phase \"qt-wrap\": look for executables in
\"bin\", \"sbin\" and \"libexec\" of all outputs and create wrappers with
suitably set environment variables if found.

Wrapping is not applied to outputs whose name is listed in
QT-WRAP-EXCLUDED-OUTPUTS.  This is useful when an output is known not
to contain any Qt binaries, and where wrapping would gratuitously
add a dependency of that output on Qt."
  (define (find-files-to-wrap output-dir)
    (append-map
     (lambda (dir)
       (if (directory-exists? dir) (find-files dir ".*") (list)))
     (list (string-append output-dir "/bin")
           (string-append output-dir "/sbin")
           (string-append output-dir "/libexec")
           (string-append output-dir "/lib/libexec"))))

  (define handle-output
    (match-lambda
     ((output . output-dir)
      (unless (member output qt-wrap-excluded-outputs)
        (for-each (cut wrap-qt-program* <>
                       #:output-dir output-dir #:inputs inputs)
                  (find-files-to-wrap output-dir))))))

  (for-each handle-output outputs)
  #t)
