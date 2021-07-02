;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2014, 2015, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (guix build qt-build-system)
  #:use-module ((guix build cmake-build-system) #:prefix cmake:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            qt-build))

;; Commentary:
;;
;; Builder-side code of the standard Qt build procedure.
;;
;; Code:

(define* (check-setup #:rest args)
  ;; Make Qt render "offscreen". In many cases this allows to run tests
  ;; without starting a X11 server.
  (setenv "QT_QPA_PLATFORM" "offscreen")
  ;; Qt/KDE tests often need dbus (`dbus-launch …`) which is not fully
  ;; set-up the the build container.
  (setenv "DBUS_FATAL_WARNINGS" "0")
  ;; Set here to ease overwriting 'check (even if set there, too)
  (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
  #t)

(define (variables-for-wrapping base-directories)

  (define (collect-sub-dirs base-directories subdirectory
                            selectors)
    ;; Append SUBDIRECTORY and each of BASE-DIRECTORIES, and return the subset
    ;; that exists and has at least one of the SELECTORS sub-directories,
    ;; unless SELECTORS is the empty list.
    (filter-map (lambda (dir)
                  (let ((directory (string-append dir subdirectory)))
                    (and (directory-exists? directory)
                         (or (null? selectors)
                             (any (lambda (selector)
                                    (directory-exists?
                                     (string-append directory selector)))
                                  selectors))
                         directory)))
                base-directories))

  (filter-map
   (match-lambda
     ((variable directory selectors ...)
      (match (collect-sub-dirs base-directories directory
                               selectors)
        (()
         #f)
        (directories
         `(,variable = ,directories)))))

   ;; These shall match the search-path-specification for Qt and KDE
   ;; libraries.
   (list '("XDG_DATA_DIRS" "/share"

           ;; These are "selectors": consider /share if and only if at least
           ;; one of these sub-directories exist.  This avoids adding
           ;; irrelevant packages to XDG_DATA_DIRS just because they have a
           ;; /share sub-directory.
           "/glib-2.0/schemas" "/sounds" "/themes"
           "/cursors" "/wallpapers" "/icons" "/mime")
         '("XDG_CONFIG_DIRS" "/etc/xdg")
         '("QT_PLUGIN_PATH" "/lib/qt5/plugins")
         '("QML2_IMPORT_PATH" "/lib/qt5/qml")
         '("QTWEBENGINEPROCESS_PATH" "/lib/qt5/libexec/QtWebEngineProcess"))))

(define* (wrap-all-programs #:key inputs outputs
                            (qt-wrap-excluded-outputs '())
                            #:allow-other-keys)
  "Implement phase \"qt-wrap\": look for GSettings schemas and
gtk+-v.0 libraries and create wrappers with suitably set environment variables
if found.

Wrapping is not applied to outputs whose name is listed in
QT-WRAP-EXCLUDED-OUTPUTS.  This is useful when an output is known not
to contain any Qt binaries, and where wrapping would gratuitously
add a dependency of that output on Qt."
  (define (find-files-to-wrap directory)
    (append-map
     (lambda (dir)
       (if (directory-exists? dir) (find-files dir ".*") (list)))
     (list (string-append directory "/bin")
           (string-append directory "/sbin")
           (string-append directory "/libexec")
           (string-append directory "/lib/libexec"))))

  (define input-directories
    ;; FIXME: Filter out unwanted inputs, e.g. cmake
    (match inputs
           (((_ . dir) ...)
            dir)))

  (define handle-output
    (match-lambda
     ((output . directory)
      (unless (member output qt-wrap-excluded-outputs)
        (let ((bin-list     (find-files-to-wrap directory))
              (vars-to-wrap (variables-for-wrapping
                             (append (list directory)
                                     input-directories))))
          (when (not (null? vars-to-wrap))
            (for-each (cut apply wrap-program <> vars-to-wrap)
                      bin-list)))))))

  (for-each handle-output outputs)
  #t)

(define %standard-phases
  (modify-phases cmake:%standard-phases
    (add-before 'check 'check-setup check-setup)
    (add-after 'install 'qt-wrap wrap-all-programs)))

(define* (qt-build #:key inputs (phases %standard-phases)
                   #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply cmake:cmake-build #:inputs inputs #:phases phases args))
