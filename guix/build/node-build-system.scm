;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2020 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2019, 2021 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2021, 2022 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2022 Liliana Marie Prikler <liliana.prikler@gmail.com>
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
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-71)
  #:export (%standard-phases
            with-atomic-json-file-replacement
            delete-dependencies
            node-build))

(define (with-atomic-json-file-replacement file proc)
  "Like 'with-atomic-file-replacement', but PROC is called with a single
argument---the result of parsing FILE's contents as json---and should a value
to be written as json to the replacement FILE."
  (with-atomic-file-replacement file
    (lambda (in out)
      (write-json (proc (read-json in)) out))))

(define* (assoc-ref* alist key #:optional default)
  "Like assoc-ref, but return DEFAULT instead of #f if no value exists."
  (match (assoc key alist)
    (#f default)
    ((_ . value) value)))

(define* (jsobject-ref obj key #:optional default)
  (match obj
    (('@ . alist) (assoc-ref* alist key default))))

(define* (alist-pop alist key #:optional (= equal?))
  "Return two values, the first pair in ALIST with key KEY, and the other
elements.  Equality calls are made as (= KEY ALISTCAR)."
  (define (found? pair)
    (= key (car pair)))

  (let ((before after (break found? alist)))
    (if (pair? after)
        (values (car after) (append before (cdr after)))
        (values #f before))))

(define* (alist-update alist key proc #:optional default (= equal?))
  "Return an association list like ALIST, but with KEY mapped to the result of
PROC applied to the first value found under the comparison (= KEY ALISTCAR).
If no such value exists, use DEFAULT instead.
Unlike acons, this removes the previous association of KEY (assuming it is
unique), but the result may still share storage with ALIST."
  (let ((pair rest (alist-pop alist key =)))
    (acons key
           (proc (if (pair? pair)
                     (cdr pair)
                     default))
           rest)))

(define (jsobject-update* js . updates)
  "Return a json object like JS, but with all UPDATES applied.  Each update is
a list (KEY PROC [DEFAULT]), so that KEY is mapped to the result of PROC
applied to the value to which KEY is mapped in JS.  If no such mapping exists,
PROC is instead applied to DEFAULT, or to '#f' is no DEFAULT is specified.
The update takes place from left to right, so later UPDATERs will receive the
values returned by earlier UPDATERs for the same KEY."
  (match js
    (('@ . alist)
     (let loop ((alist alist)
                (updates updates))
       (match updates
         (() (cons '@ alist))
         (((key proc) . updates)
          (loop (alist-update alist key proc #f equal?) updates))
         (((key proc default) . updates)
          (loop (alist-update alist key proc default equal?) updates)))))))

(define (jsobject-union combine seed . objects)
  "Merge OBJECTS into SEED by applying (COMBINE KEY VAL0 VAL), where VAL0
is the value found in the (possibly updated) SEED and VAL is the new value
found in one of the OBJECTS."
  (match seed
    (('@ . aseed)
     (match objects
       (() seed)
       ((('@ . alists) ...)
        (cons
         '@
         (fold (lambda (alist aseed)
                 (if (null? aseed) alist
                     (fold
                      (match-lambda*
                        (((k . v) aseed)
                         (let ((pair tail (alist-pop alist k)))
                           (match pair
                             (#f (acons k v aseed))
                             ((_ . v0) (acons k (combine k v0 v) aseed))))))
                      aseed
                      alist)))
               aseed
               alists)))))))

;; Possibly useful helper functions:
;; (define (newest key val0 val) val)
;; (define (unkeyed->keyed proc) (lambda (_key val0 val) (proc val0 val)))


;;;
;;; Phases.
;;;

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
    (jsobject-ref package-meta "name")))

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

  (define resolve-dependencies
    (match-lambda
      (('@ . alist)
       (cons '@ (map (match-lambda
                       ((key . value)
                        (cons key (hash-ref index key value))))
                     alist)))))

  (with-atomic-json-file-replacement "package.json"
    (lambda (pkg-meta)
      (jsobject-update*
       pkg-meta
       `("devDependencies" ,resolve-dependencies (@))
       `("dependencies" ,(lambda (deps)
                           (resolve-dependencies
                            (jsobject-union
                             (lambda (k a b) b)
                             (jsobject-ref pkg-meta "peerDependencies" '(@))
                             deps)))
         (@)))))
  #t)

(define (delete-dependencies absent)
  "Rewrite 'package.json' to allow the build to proceed without packages
listed in ABSENT, a list of strings naming npm packages.

To prevent the deleted dependencies from being reintroduced, use this function
only after the 'patch-dependencies' phase."
  (define delete-from-jsobject
    (match-lambda
      (('@ . alist)
       (cons '@ (filter (match-lambda
                          ((k . _)
                           (not (member k absent))))
                        alist)))))

  (with-atomic-json-file-replacement "package.json"
    (lambda (pkg-meta)
      (jsobject-update*
       pkg-meta
       `("devDependencies" ,delete-from-jsobject (@))
       `("dependencies" ,delete-from-jsobject (@))))))

(define* (delete-lockfiles #:key inputs #:allow-other-keys)
  "Delete 'package-lock.json', 'yarn.lock', and 'npm-shrinkwrap.json', if they
exist."
  (for-each (lambda (pth)
              (when (file-exists? pth)
                (delete-file pth)))
            '("package-lock.json"
              "yarn.lock"
              "npm-shrinkwrap.json"))
  #t)

(define* (configure #:key outputs inputs #:allow-other-keys)
  (let ((npm (string-append (assoc-ref inputs "node") "/bin/npm")))
    (invoke npm "--offline" "--ignore-scripts" "install")
    #t))

(define* (build #:key inputs #:allow-other-keys)
  (let ((package-meta (call-with-input-file "package.json" read-json)))
    (if (jsobject-ref (jsobject-ref package-meta "scripts" '(@)) "build" #f)
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
  (invoke "tar"
          ;; Add options suggested by https://reproducible-builds.org/docs/archives/
          "--sort=name"
          (string-append "--mtime=@" (getenv "SOURCE_DATE_EPOCH"))
          "--owner=0"
          "--group=0"
          "--numeric-owner"
          "-czf" "../package.tgz" ".")
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

(define* (avoid-node-gyp-rebuild #:key outputs #:allow-other-keys)
  "Adjust the installed 'package.json' to remove an 'install' script that
would try to run 'node-gyp rebuild'."
  ;; We want to take advantage of `npm install`'s automatic support for
  ;; building native addons with node-gyp: in particular, it helps us avoid
  ;; hard-coding the specifics of how npm's internal copy of node-gyp is
  ;; currently packaged. However, the mechanism by which the automatic support
  ;; is implemented causes problems for us.
  ;;
  ;; If a package contains a 'binding.gyp' file and does not define an
  ;; 'install' or 'preinstall' script, 'npm install' runs a default install
  ;; script consisting of 'node-gyp rebuild'. In our 'install' phase, this
  ;; implicit 'install' script, if it is applicable, is explicitly added to
  ;; the "package.json" file. However, if another Guix package were to use a
  ;; Node.js package with such an 'install' script, the dependent package's
  ;; build process would fail, because 'node-gyp rebuild' would try to write
  ;; to the store.
  ;;
  ;; Here, if the installed "package.json" defines scripts.install as
  ;; "node-gyp rebuild", we replace it with a no-op. Importantly, deleting the
  ;; install script definition would not be enough, because the default
  ;; install script would cause the same problem.
  ;;
  ;; For further details, see:
  ;; - https://docs.npmjs.com/cli/v8/configuring-npm/package-json#default-values
  ;; - https://docs.npmjs.com/cli/v8/using-npm/scripts#best-practices
  (define installed-package.json
    (search-input-file outputs (string-append "/lib/node_modules/"
                                              (module-name ".")
                                              "/package.json")))
  ;; We don't want to use an atomic replacement here, because we often don't
  ;; even need to overwrite this file.  Therefore, let's use some helpers
  ;; that we'd otherwise not need.
  (define pkg-meta
    (call-with-input-file installed-package.json read-json))
  (define scripts
    (jsobject-ref pkg-meta "scripts" '(@)))
  (define (jsobject-set js key val)
    (jsobject-update* js (list key (const val))))

  (when (equal? "node-gyp rebuild" (jsobject-ref scripts "install" #f))
    (call-with-output-file installed-package.json
      (lambda (out)
        (write-json
         (jsobject-set pkg-meta
                       "scripts"
                       (jsobject-set scripts
                                     "install"
                                     "echo Guix: avoiding node-gyp rebuild"))
         out)))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (add-after 'unpack 'set-home set-home)
    (add-before 'configure 'patch-dependencies patch-dependencies)
    (add-after 'patch-dependencies 'delete-lockfiles delete-lockfiles)
    (replace 'configure configure)
    (replace 'build build)
    (replace 'check check)
    (add-before 'install 'repack repack)
    (replace 'install install)
    (add-after 'install 'avoid-node-gyp-rebuild avoid-node-gyp-rebuild)))

(define* (node-build #:key inputs (phases %standard-phases)
                     #:allow-other-keys #:rest args)
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
