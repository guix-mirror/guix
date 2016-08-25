;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
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
  #:export (wrap-qt-program))

(define (wrap-qt-program out program)
  (define (suffix env-var path)
    (let ((env-val (getenv env-var)))
      (if env-val (string-append env-val ":" path) path)))

  (let ((qml-path        (suffix "QML2_IMPORT_PATH"
                                 (string-append out "/qml")))
        (plugin-path     (suffix "QT_PLUGIN_PATH"
                                 (string-append out "/plugins")))
        (xdg-data-path   (suffix "XDG_DATA_DIRS"
                                 (string-append out "/share")))
        (xdg-config-path (suffix "XDG_CONFIG_DIRS"
                                 (string-append out "/etc/xdg"))))
    (wrap-program (string-append out "/bin/" program)
      `("QML2_IMPORT_PATH" = (,qml-path))
      `("QT_PLUGIN_PATH" = (,plugin-path))
      `("XDG_DATA_DIRS" = (,xdg-data-path))
      `("XDG_CONFIG_DIRS" = (,xdg-config-path)))))
