;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages presentation)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (gnu packages figlet)
  #:use-module (gnu packages games)
  #:use-module (gnu packages image)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define-public presentty
  (package
    (name "presentty")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "presentty" version))
        (sha256
         (base32
          "1qpy992hyg1amjl0acic3agj20spcpv5m0ncg1283mmxs8cs3xy9"))
        (patches
          (list
            (origin
              (method url-fetch)
              (uri "https://sources.debian.org/data/main/p/presentty/0.2.1-1/debian/patches/presentty-python3.patch")
              (sha256
               (base32
                "03d3ylh1z99g4dqj7aka60spagnwss9mbacd7jbpk1gazflnssz1")))))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; Test suite hasn't withstood the test of time.
       #:phases
       (modify-phases %standard-phases
         (replace 'wrap
           (lambda* (#:key python inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/"))
                    (python (assoc-ref inputs "python")))
               (for-each
                 (lambda (program)
                   (wrap-program (string-append bin program)
                     `("PATH" ":" prefix (,(dirname (which "cowsay"))
                                          ,(dirname (which "figlet"))
                                          ,(dirname (which "jp2a"))))
                     `("GUIX_PYTHONPATH" prefix
                       ,(cons (string-append out "/lib/python"
                                             (python-version python)
                                             "/site-packages")
                              (search-path-as-string->list
                                (or (getenv "GUIX_PYTHONPATH") ""))))))
                 '("presentty" "presentty-console")))
             #t)))))
    (inputs
     (list cowsay
           figlet
           jp2a
           python-docutils
           python-pillow-2.9
           python-six
           python-urwid))
    (native-inputs
     (list python-pbr python-pygments))
    (home-page "http://git.inaugust.com/cgit/presentty/")
    (synopsis "Console-based presentation system")
    (description "Presentty is a console-based presentation program where slides
are authored in reStructuredText.  Its features include, but are not limited to:
Cross-fade animations, progressive list display, panning transitions, syntax
highlighting, Cowsay and figlet integration, ANSI art, JPEG display.")
    (license license:gpl3+)))
