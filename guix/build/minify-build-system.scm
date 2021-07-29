;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix build minify-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module ((guix build minify-build-system) #:prefix minify:)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:export (%standard-phases
            minify-build
            minify))

;; Commentary:
;;
;; Builder-side code of the standard minification procedure for JavaScript
;; files.
;;
;; Code:

(define* (minify file #:key target (directory ""))
  (format #t "minifying ~a\n" file)
  (let* ((base (basename file ".js"))
         (installed (or target (string-append directory base ".min.js")))
         (minified (open-pipe* OPEN_READ "uglifyjs" file)))
    (call-with-output-file installed
      (cut dump-port minified <>))
    (match (close-pipe minified)
      (0 #t)
      (status
       (error "uglify-js failed" status)))))

(define* (build #:key javascript-files
                #:allow-other-keys)
  (let ((files (or javascript-files
                   (find-files "src" "\\.js$"))))
    (mkdir-p "guix/build")
    (for-each (cut minify <> #:directory "guix/build/") files)))

(define* (install #:key outputs #:allow-other-keys)
  (let* ((out (assoc-ref outputs "out"))
         (js  (string-append out "/share/javascript/")))
    (mkdir-p js)
    (for-each
      (lambda (file)
        (if (not (zero? (stat:size (stat file))))
          (install-file file js)
          (error "File is empty: " file)))
      (find-files "guix/build" "\\.min\\.js$"))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (replace 'build build)
    (delete 'check)
    (replace 'install install)))

(define* (minify-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  "Build the given JavaScript package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; minify-build-system.scm ends here
