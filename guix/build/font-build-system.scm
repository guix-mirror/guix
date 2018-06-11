;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Alex Griffin <a@ajgrf.com>
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

(define-module (guix build font-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            font-build))

;; Commentary:
;;
;; Builder-side code of the build procedure for font packages.
;;
;; Code:

(define gnu:unpack (assoc-ref gnu:%standard-phases 'unpack))

(define* (unpack #:key source #:allow-other-keys)
  "Unpack SOURCE into the build directory.  SOURCE may be a compressed
archive, or a font file."
  (if (any (cut string-suffix? <> source)
           (list ".ttf" ".otf"))
      (begin
        (mkdir "source")
        (chdir "source")
        (copy-file source (strip-store-file-name source))
        #t)
      (gnu:unpack #:source source)))

(define* (install #:key outputs #:allow-other-keys)
  "Install the package contents."
  (let* ((out (assoc-ref outputs "out"))
         (source (getcwd))
         (fonts (string-append out "/share/fonts")))
    (for-each (cut install-file <> (string-append fonts "/truetype"))
              (find-files source "\\.(ttf|ttc)$"))
    (for-each (cut install-file <> (string-append fonts "/opentype"))
              (find-files source "\\.(otf|otc)$"))
    #t))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (replace 'unpack unpack)
    (delete 'bootstrap)
    (delete 'configure)
    (delete 'check)
    (delete 'build)
    (replace 'install install)))

(define* (font-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given font package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; font-build-system.scm ends here
