;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2020 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2019, 2021 Timothy Sample <samplet@ngyro.com>
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

(define-module (guix build node-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (guix build json)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%standard-phases
            node-build))

;; Commentary:
;;
;; Builder-side code of the standard Node/NPM package install procedure.
;;
;; Code:

(define (set-home . _)
  (with-directory-excursion ".."
    (let loop ((i 0))
      (let ((dir (string-append "npm-home-" (number->string i))))
        (if (directory-exists? dir)
            (loop (1+ i))
            (begin
              (mkdir dir)
              (setenv "HOME" (string-append (getcwd) "/" dir))
              (format #t "set HOME to ~s~%" (getenv "HOME")))))))
  #t)

(define (module-name module)
  (let* ((package.json (string-append module "/package.json"))
         (package-meta (call-with-input-file package.json read-json)))
    (assoc-ref package-meta "name")))

(define (index-modules input-paths)
  (define (list-modules directory)
    (append-map (lambda (x)
                  (if (string-prefix? "@" x)
                      (list-modules (string-append directory "/" x))
                      (list (string-append directory "/" x))))
                (filter (lambda (x)
                          (not (member x '("." ".."))))
                        (or (scandir directory) '()))))
  (let ((index (make-hash-table (* 2 (length input-paths)))))
    (for-each (lambda (dir)
                (let ((nm (string-append dir "/lib/node_modules")))
                  (for-each (lambda (module)
                              (hash-set! index (module-name module) module))
                            (list-modules nm))))
              input-paths)
    index))

(define* (patch-dependencies #:key inputs #:allow-other-keys)

  (define index (index-modules (map cdr inputs)))

  (define (resolve-dependencies package-meta meta-key)
    (fold (lambda (key+value acc)
            (match key+value
              ('@ acc)
              ((key . value) (acons key (hash-ref index key value) acc))))
          '()
          (or (assoc-ref package-meta meta-key) '())))

  (with-atomic-file-replacement "package.json"
    (lambda (in out)
      (let ((package-meta (read-json in)))
        (assoc-set! package-meta "dependencies"
                    (append
                     '(@)
                     (resolve-dependencies package-meta "dependencies")
                     (resolve-dependencies package-meta "peerDependencies")))
        (assoc-set! package-meta "devDependencies"
                    (append
                     '(@)
                     (resolve-dependencies package-meta "devDependencies")))
        (write-json package-meta out))))
  #t)

(define* (configure #:key outputs inputs #:allow-other-keys)
  (let ((npm (string-append (assoc-ref inputs "node") "/bin/npm")))
    (invoke npm "--offline" "--ignore-scripts" "install")
    #t))

(define* (build #:key inputs #:allow-other-keys)
  (let ((package-meta (call-with-input-file "package.json" read-json)))
    (if (and=> (assoc-ref package-meta "scripts")
               (lambda (scripts)
                 (assoc-ref scripts "build")))
        (let ((npm (string-append (assoc-ref inputs "node") "/bin/npm")))
          (invoke npm "run" "build"))
        (format #t "there is no build script to run~%"))
    #t))

(define* (check #:key tests? inputs #:allow-other-keys)
  "Run 'npm test' if TESTS?"
  (if tests?
      (let ((npm (string-append (assoc-ref inputs "node") "/bin/npm")))
        (invoke npm "test"))
      (format #t "test suite not run~%"))
  #t)

(define* (repack #:key inputs #:allow-other-keys)
  (invoke "tar" "-czf" "../package.tgz" ".")
  #t)

(define* (install #:key outputs inputs #:allow-other-keys)
  "Install the node module to the output store item."
  (let ((out (assoc-ref outputs "out"))
        (npm (string-append (assoc-ref inputs "node") "/bin/npm")))
    (invoke npm "--prefix" out
            "--global"
            "--offline"
            "--loglevel" "info"
            "--production"
            "install" "../package.tgz")
    #t))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (add-after 'unpack 'set-home set-home)
    (add-before 'configure 'patch-dependencies patch-dependencies)
    (replace 'configure configure)
    (replace 'build build)
    (replace 'check check)
    (add-before 'install 'repack repack)
    (replace 'install install)))

(define* (node-build #:key inputs (phases %standard-phases)
                     #:allow-other-keys #:rest args)
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
