;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Vinicius Monego <monego@posteo.net>
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

(define-module (gnu packages orange)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt))

(define-public python-orange-canvas-core
  (package
    (name "python-orange-canvas-core")
    (version "0.1.19")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "orange-canvas-core" version))
       (sha256
        (base32 "03wav2msfm32y8zwq69v1v6qyh1ld76xla2z60avf49yhbwjgwal"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; This test fails with: RuntimeError: Event loop is closed.
             (substitute* "orangecanvas/application/tests/test_mainwindow.py"
               (("test_help_requests") "_test_help_requests"))
             (setenv "HOME" "/tmp")
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (propagated-inputs
     `(("python-anyqt" ,python-anyqt)
       ("python-cachecontrol" ,python-cachecontrol)
       ("python-commonmark" ,python-commonmark)
       ("python-dictdiffer" ,python-dictdiffer)
       ("python-docutils" ,python-docutils)
       ("python-pyqt" ,python-pyqt)
       ("python-qasync" ,python-qasync)
       ("python-requests" ,python-requests)))
    (home-page "https://github.com/biolab/orange-canvas-core")
    (synopsis "Core component of Orange Canvas")
    (description
     "Orange Canvas Core is a framework for building graphical user interfaces
for editing workflows.  It is a component used to build the Orange Canvas
data-mining application.")
    (license license:gpl3)))

(define-public python-orange-widget-base
  (package
    (name "python-orange-widget-base")
    (version "4.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "orange-widget-base" version))
       (sha256
        (base32 "13sy3s8rdqs3i3ghixljpqvnfz81qmbb0kqlasw39zvf39qc22kz"))))
    (build-system python-build-system)
    (arguments
     `(;; unittest fails to load one test, all other tests are passing:
       ;; AttributeError: module 'orangewidget' has no attribute 'version'.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" "/tmp")
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (propagated-inputs
     `(("python-anyqt" ,python-anyqt)
       ("python-matplotlib" ,python-matplotlib)
       ("python-orange-canvas-core"
        ,python-orange-canvas-core)
       ("python-pyqtgraph" ,python-pyqtgraph)))
    (home-page "https://github.com/biolab/orange-widget-base")
    (synopsis "Base Widget for Orange Canvas")
    (description
     "Orange Widget Base provides a base widget component for a interactive
GUI based workflow.  It is primarily used in the Orange framework.")
    (license license:gpl3+)))

(define-public python-serverfiles
  (package
    (name "python-serverfiles")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "serverfiles" version))
       (sha256
        (base32 "1qgbzgnaxj4wsp2rdas53qxsh0j7xpibq25w6lviwyaqwwrgq42y"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-requests" ,python-requests)))
    (home-page "https://github.com/biolab/serverfiles")
    (synopsis "Utility to access files on a HTTP server and store them locally")
    (description
     "This package provides an utility that accesses files on a HTTP server
and stores them locally for reuse.  It is primarily used by the Orange
framework.")
    (license license:gpl3+)))
