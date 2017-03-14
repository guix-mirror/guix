;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts pack)
  #:use-module (guix scripts)
  #:use-module (guix ui)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix store)
  #:use-module (guix grafts)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix derivations)
  #:use-module (guix scripts build)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:autoload   (gnu packages base) (tar)
  #:autoload   (gnu packages package-management) (guix)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:export (compressor?
            lookup-compressor
            self-contained-tarball
            guix-pack))

;; Type of a compression tool.
(define-record-type <compressor>
  (compressor name package extension command)
  compressor?
  (name       compressor-name)                    ;string (e.g., "gzip")
  (package    compressor-package)                 ;package
  (extension  compressor-extension)               ;string (e.g., "lz")
  (command    compressor-command))                ;list (e.g., '("gzip" "-9n"))

(define %compressors
  ;; Available compression tools.
  (list (compressor "gzip"  gzip  "gz"  '("gzip" "-9n"))
        (compressor "lzip"  lzip  "lz"  '("lzip" "-9"))
        (compressor "xz"    xz    "xz"  '("xz" "-e"))
        (compressor "bzip2" bzip2 "bz2" '("bzip2" "-9"))))

(define (lookup-compressor name)
  "Return the compressor object called NAME.  Error out if it could not be
found."
  (or (find (match-lambda
              (($ <compressor> name*)
               (string=? name* name)))
            %compressors)
      (leave (_ "~a: compressor not found~%") name)))

(define* (self-contained-tarball name profile
                                 #:key deduplicate?
                                 (compressor (first %compressors))
                                 localstatedir?
                                 (symlinks '())
                                 (tar tar))
  "Return a self-contained tarball containing a store initialized with the
closure of PROFILE, a derivation.  The tarball contains /gnu/store; if
LOCALSTATEDIR? is true, it also contains /var/guix, including /var/guix/db
with a properly initialized store database.

SYMLINKS must be a list of (SOURCE -> TARGET) tuples denoting symlinks to be
added to the pack."
  (define build
    (with-imported-modules '((guix build utils)
                             (guix build store-copy)
                             (gnu build install))
      #~(begin
          (use-modules (guix build utils)
                       (gnu build install)
                       (srfi srfi-1)
                       (srfi srfi-26)
                       (ice-9 match))

          (define %root "root")

          (define symlink->directives
            ;; Return "populate directives" to make the given symlink and its
            ;; parent directories.
            (match-lambda
              ((source '-> target)
               (let ((target (string-append #$profile "/" target)))
                 `((directory ,(dirname source))
                   (,source -> ,target))))))

          (define directives
            ;; Fully-qualified symlinks.
            (append-map symlink->directives '#$symlinks))

          ;; The --sort option was added to GNU tar in version 1.28, released
          ;; 2014-07-28.  For testing, we use the bootstrap tar, which is
          ;; older and doesn't support it.
          (define tar-supports-sort?
            (zero? (system* (string-append #+tar "/bin/tar")
                            "cf" "/dev/null" "--files-from=/dev/null"
                            "--sort=name")))

          ;; We need Guix here for 'guix-register'.
          (setenv "PATH"
                  (string-append #$(if localstatedir?
                                       (file-append guix "/sbin:")
                                       "")
                                 #$tar "/bin:"
                                 #$(compressor-package compressor) "/bin"))

          ;; Note: there is not much to gain here with deduplication and
          ;; there is the overhead of the '.links' directory, so turn it
          ;; off.
          (populate-single-profile-directory %root
                                             #:profile #$profile
                                             #:closure "profile"
                                             #:deduplicate? #f
                                             #:register? #$localstatedir?)

          ;; Create SYMLINKS.
          (for-each (cut evaluate-populate-directive <> %root)
                    directives)

          ;; Create the tarball.  Use GNU format so there's no file name
          ;; length limitation.
          (with-directory-excursion %root
            (exit
             (zero? (apply system* "tar"
                           "-I" #$(string-join (compressor-command compressor))
                           "--format=gnu"

                           ;; Avoid non-determinism in the archive.  Use
                           ;; mtime = 1, not zero, because that is what the
                           ;; daemon does for files in the store (see the
                           ;; 'mtimeStore' constant in local-store.cc.)
                           (if tar-supports-sort? "--sort=name" "--mtime=@1")
                           "--mtime=@1"           ;for files in /var/guix
                           "--owner=root:0"
                           "--group=root:0"

                           "--check-links"
                           "-cvf" #$output
                           ;; Avoid adding / and /var to the tarball, so
                           ;; that the ownership and permissions of those
                           ;; directories will not be overwritten when
                           ;; extracting the archive.  Do not include /root
                           ;; because the root account might have a
                           ;; different home directory.
                           #$@(if localstatedir?
                                  '("./var/guix")
                                  '())

                           (string-append "." (%store-directory))

                           (delete-duplicates
                            (filter-map (match-lambda
                                          (('directory directory)
                                           (string-append "." directory))
                                          (_ #f))
                                        directives)))))))))

  (gexp->derivation (string-append name ".tar."
                                   (compressor-extension compressor))
                    build
                    #:references-graphs `(("profile" ,profile))))



;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  `((system . ,(%current-system))
    (substitutes? . #t)
    (graft? . #t)
    (max-silent-time . 3600)
    (verbosity . 0)
    (symlinks . ())
    (compressor . ,(first %compressors))))

(define %options
  ;; Specifications of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix pack")))

         (option '(#\n "dry-run") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'dry-run? #t (alist-cons 'graft? #f result))))
         (option '(#\s "system") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'system arg
                               (alist-delete 'system result eq?))))
         (option '(#\C "compression") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'compressor (lookup-compressor arg)
                               result)))
         (option '(#\S "symlink") #t #f
                 (lambda (opt name arg result)
                   (match (string-tokenize arg
                                           (char-set-complement
                                            (char-set #\=)))
                     ((source target)
                      (let ((symlinks (assoc-ref result 'symlinks)))
                        (alist-cons 'symlinks
                                    `((,source -> ,target) ,@symlinks)
                                    (alist-delete 'symlinks result eq?))))
                     (x
                      (leave (_ "~a: invalid symlink specification~%")
                             arg)))))
         (option '("localstatedir") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'localstatedir? #t result)))

         (append %transformation-options
                 %standard-build-options)))

(define (show-help)
  (display (_ "Usage: guix pack [OPTION]... PACKAGE...
Create a bundle of PACKAGE.\n"))
  (show-build-options-help)
  (newline)
  (show-transformation-options-help)
  (newline)
  (display (_ "
  -s, --system=SYSTEM    attempt to build for SYSTEM--e.g., \"i686-linux\""))
  (display (_ "
  -C, --compression=TOOL compress using TOOL--e.g., \"lzip\""))
  (display (_ "
  -S, --symlink=SPEC     create symlinks to the profile according to SPEC"))
  (display (_ "
      --localstatedir    include /var/guix in the resulting pack"))
  (newline)
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))


;;;
;;; Entry point.
;;;

(define (guix-pack . args)
  (define opts
    (parse-command-line args %options (list %default-options)))

  (with-error-handling
    (parameterize ((%graft? (assoc-ref opts 'graft?)))
      (let* ((dry-run? (assoc-ref opts 'dry-run?))
             (specs    (filter-map (match-lambda
                                     (('argument . name)
                                      name)
                                     (x #f))
                                   opts))
             (packages (map (lambda (spec)
                              (call-with-values
                                  (lambda ()
                                    (specification->package+output spec))
                                list))
                            specs))
             (compressor (assoc-ref opts 'compressor))
             (symlinks   (assoc-ref opts 'symlinks))
             (localstatedir? (assoc-ref opts 'localstatedir?)))
        (with-store store
          (run-with-store store
            (mlet* %store-monad ((profile (profile-derivation
                                           (packages->manifest packages)))
                                 (drv (self-contained-tarball "pack" profile
                                                              #:compressor
                                                              compressor
                                                              #:symlinks
                                                              symlinks
                                                              #:localstatedir?
                                                              localstatedir?)))
              (mbegin %store-monad
                (show-what-to-build* (list drv)
                                     #:use-substitutes?
                                     (assoc-ref opts 'substitutes?)
                                     #:dry-run? dry-run?)
                (munless dry-run?
                  (built-derivations (list drv))
                  (return (format #t "~a~%"
                                  (derivation->output-path drv))))))
            #:system (assoc-ref opts 'system)))))))
