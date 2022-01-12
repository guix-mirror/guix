;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Marius Bakke <marius@gnu.org>
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

(define-module (gnu packages browser-extensions)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu build chromium-extension)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python))

(define play-to-kodi
  (package
    (name "play-to-kodi")
    (version "1.9.1")
    (home-page "https://github.com/khloke/play-to-xbmc-chrome")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01rmcpbkn9vhcd8mrah2jmd2801k2r5fz7aqvp22hbwmh2z5f1ch"))))
    (build-system copy-build-system)
    (synopsis "Send website contents to Kodi")
    (description
     "Play to Kodi is a browser add-on that can send video, audio, and other
supported content to the Kodi media center.")
    (license license:expat)))

(define-public play-to-kodi/chromium
  (make-chromium-extension play-to-kodi))

(define ublock-origin
  (package
    (name "ublock-origin")
    (version "1.40.8")
    (home-page "https://github.com/gorhill/uBlock")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)
                                  (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17pywblp87npj5z3zyvy9rmllnxhm916fnfyyfbvfbczswkbp78s"))))
    (build-system gnu-build-system)
    (outputs '("xpi" "firefox" "chromium"))
    (arguments
     '(#:tests? #f                      ;no tests
       #:allowed-references ()
       #:phases
       (modify-phases (map (lambda (phase)
                             (assq phase %standard-phases))
                           '(set-paths unpack patch-source-shebangs))
         (add-after 'unpack 'do-not-depend-on-git
           (lambda _
             ;; The script attempts to checkout the uAssets submodule,
             ;; but we already did so with git-fetch.
             (substitute* "tools/make-assets.sh"
               (("^git submodule update.*")
                ""))))
         (add-after 'unpack 'make-files-writable
           (lambda _
             ;; The build system copies some files and later tries
             ;; modifying them.
             (for-each make-file-writable (find-files "."))))
         (add-after 'patch-source-shebangs 'build-xpi
           (lambda _
             (invoke "./tools/make-firefox.sh" "all")))
         (add-after 'build-xpi 'build-chromium
           (lambda _
             (invoke "./tools/make-chromium.sh")))
         (add-after 'build-chromium 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((firefox (assoc-ref outputs "firefox"))
                   (xpi (assoc-ref outputs "xpi"))
                   (chromium (assoc-ref outputs "chromium")))
               (install-file "dist/build/uBlock0.firefox.xpi"
                             (string-append xpi "/lib/mozilla/extensions"))
               (copy-recursively "dist/build/uBlock0.firefox" firefox)
               (copy-recursively "dist/build/uBlock0.chromium" chromium)))))))
    (native-inputs
     (list python-wrapper zip))
    (synopsis "Block unwanted content from web sites")
    (description
     "uBlock Origin is a @dfn{wide spectrum blocker} for IceCat and
ungoogled-chromium.")
    (license license:gpl3+)))

(define-public ublock-origin/chromium
  (make-chromium-extension ublock-origin "chromium"))
