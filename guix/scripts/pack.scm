;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;; Copyright © 2018 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module ((guix status) #:select (with-status-verbosity))
  #:use-module (guix grafts)
  #:autoload   (guix inferior) (inferior-package?)
  #:use-module (guix monads)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix describe)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system gnu)
  #:use-module (guix scripts build)
  #:use-module ((guix self) #:select (make-config.scm))
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module ((gnu packages compression) #:hide (zip))
  #:use-module (gnu packages guile)
  #:use-module (gnu packages base)
  #:autoload   (gnu packages package-management) (guix)
  #:autoload   (gnu packages gnupg) (guile-gcrypt)
  #:autoload   (gnu packages guile) (guile2.0-json guile-json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:export (compressor?
            lookup-compressor
            self-contained-tarball
            docker-image
            squashfs-image

            guix-pack))

;; Type of a compression tool.
(define-record-type <compressor>
  (compressor name extension command)
  compressor?
  (name       compressor-name)      ;string (e.g., "gzip")
  (extension  compressor-extension) ;string (e.g., ".lz")
  (command    compressor-command))  ;gexp (e.g., #~("/gnu/store/…/gzip" "-9n"))

(define %compressors
  ;; Available compression tools.
  (list (compressor "gzip"  ".gz"
                    #~(#+(file-append gzip "/bin/gzip") "-9n"))
        (compressor "lzip"  ".lz"
                    #~(#+(file-append lzip "/bin/lzip") "-9"))
        (compressor "xz"    ".xz"
                    #~(#+(file-append xz "/bin/xz") "-e"))
        (compressor "bzip2" ".bz2"
                    #~(#+(file-append bzip2 "/bin/bzip2") "-9"))
        (compressor "none" "" #f)))

;; This one is only for use in this module, so don't put it in %compressors.
(define bootstrap-xz
  (compressor "bootstrap-xz" ".xz"
              #~(#+(file-append %bootstrap-coreutils&co "/bin/xz") "-e")))

(define (lookup-compressor name)
  "Return the compressor object called NAME.  Error out if it could not be
found."
  (or (find (match-lambda
              (($ <compressor> name*)
               (string=? name* name)))
            %compressors)
      (leave (G_ "~a: compressor not found~%") name)))

(define not-config?
  ;; Select (guix …) and (gnu …) modules, except (guix config).
  (match-lambda
    (('guix 'config) #f)
    (('guix _ ...) #t)
    (('gnu _ ...) #t)
    (_ #f)))

(define gcrypt-sqlite3&co
  ;; Guile-Gcrypt, Guile-SQLite3, and their propagated inputs.
  (append-map (lambda (package)
                (cons package
                      (match (package-transitive-propagated-inputs package)
                        (((labels packages) ...)
                         packages))))
              (list guile-gcrypt guile-sqlite3)))

(define (store-database items)
  "Return a directory containing a store database where all of ITEMS and their
dependencies are registered."
  (define schema
    (local-file (search-path %load-path
                             "guix/store/schema.sql")))


  (define labels
    (map (lambda (n)
           (string-append "closure" (number->string n)))
         (iota (length items))))

  (define build
    (with-extensions gcrypt-sqlite3&co
      ;; XXX: Adding (gnu build install) just to work around
      ;; <https://bugs.gnu.org/15602>: that way, (guix build store-copy) is
      ;; copied last and the 'store-info-XXX' macros are correctly expanded.
      (with-imported-modules (source-module-closure
                              '((guix build store-copy)
                                (guix store database)
                                (gnu build install)))
        #~(begin
            (use-modules (guix store database)
                         (guix build store-copy)
                         (srfi srfi-1))

            (define (read-closure closure)
              (call-with-input-file closure read-reference-graph))

            (let ((items (append-map read-closure '#$labels)))
              (register-items items
                              #:state-directory #$output
                              #:deduplicate? #f
                              #:reset-timestamps? #f
                              #:registration-time %epoch
                              #:schema #$schema))))))

  (computed-file "store-database" build
                 #:options `(#:references-graphs ,(zip labels items))))

(define* (self-contained-tarball name profile
                                 #:key target
                                 (profile-name "guix-profile")
                                 deduplicate?
                                 (compressor (first %compressors))
                                 localstatedir?
                                 (symlinks '())
                                 (archiver tar))
  "Return a self-contained tarball containing a store initialized with the
closure of PROFILE, a derivation.  The tarball contains /gnu/store; if
LOCALSTATEDIR? is true, it also contains /var/guix, including /var/guix/db
with a properly initialized store database.

SYMLINKS must be a list of (SOURCE -> TARGET) tuples denoting symlinks to be
added to the pack."
  (define database
    (and localstatedir?
         (file-append (store-database (list profile))
                      "/db/db.sqlite")))

  (define build
    (with-imported-modules (source-module-closure
                            `((guix build utils)
                              (guix build union)
                              (gnu build install))
                            #:select? not-config?)
      #~(begin
          (use-modules (guix build utils)
                       ((guix build union) #:select (relative-file-name))
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
               (let ((target (string-append #$profile "/" target))
                     (parent (dirname source)))
                 ;; Never add a 'directory' directive for "/" so as to
                 ;; preserve its ownnership when extracting the archive (see
                 ;; below), and also because this would lead to adding the
                 ;; same entries twice in the tarball.
                 `(,@(if (string=? parent "/")
                         '()
                         `((directory ,parent)))
                   (,source
                    -> ,(relative-file-name parent target)))))))

          (define directives
            ;; Fully-qualified symlinks.
            (append-map symlink->directives '#$symlinks))

          ;; The --sort option was added to GNU tar in version 1.28, released
          ;; 2014-07-28.  For testing, we use the bootstrap tar, which is
          ;; older and doesn't support it.
          (define tar-supports-sort?
            (zero? (system* (string-append #+archiver "/bin/tar")
                            "cf" "/dev/null" "--files-from=/dev/null"
                            "--sort=name")))

          ;; Add 'tar' to the search path.
          (setenv "PATH" #+(file-append archiver "/bin"))

          ;; Note: there is not much to gain here with deduplication and there
          ;; is the overhead of the '.links' directory, so turn it off.
          ;; Furthermore GNU tar < 1.30 sometimes fails to extract tarballs
          ;; with hard links:
          ;; <http://lists.gnu.org/archive/html/bug-tar/2017-11/msg00009.html>.
          (populate-single-profile-directory %root
                                             #:profile #$profile
                                             #:profile-name #$profile-name
                                             #:closure "profile"
                                             #:database #+database)

          ;; Create SYMLINKS.
          (for-each (cut evaluate-populate-directive <> %root)
                    directives)

          ;; Create the tarball.  Use GNU format so there's no file name
          ;; length limitation.
          (with-directory-excursion %root
            (exit
             (zero? (apply system* "tar"
                           #+@(if (compressor-command compressor)
                                  #~("-I"
                                     (string-join
                                      '#+(compressor-command compressor)))
                                  #~())
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
                                          ((source '-> _)
                                           (string-append "." source))
                                          (_ #f))
                                        directives)))))))))

  (gexp->derivation (string-append name ".tar"
                                   (compressor-extension compressor))
                    build
                    #:references-graphs `(("profile" ,profile))))

(define* (squashfs-image name profile
                         #:key target
                         (profile-name "guix-profile")
                         (compressor (first %compressors))
                         localstatedir?
                         (symlinks '())
                         (archiver squashfs-tools-next))
  "Return a squashfs image containing a store initialized with the closure of
PROFILE, a derivation.  The image contains a subset of /gnu/store, empty mount
points for virtual file systems (like procfs), and optional symlinks.

SYMLINKS must be a list of (SOURCE -> TARGET) tuples denoting symlinks to be
added to the pack."
  (define database
    (and localstatedir?
         (file-append (store-database (list profile))
                      "/db/db.sqlite")))

  (define build
    (with-imported-modules (source-module-closure
                            '((guix build utils)
                              (guix build store-copy)
                              (guix build union)
                              (gnu build install))
                            #:select? not-config?)
      #~(begin
          (use-modules (guix build utils)
                       (guix build store-copy)
                       ((guix build union) #:select (relative-file-name))
                       (gnu build install)
                       (srfi srfi-1)
                       (srfi srfi-26)
                       (ice-9 match))

          (define database #+database)

          (setenv "PATH" (string-append #$archiver "/bin"))

          ;; We need an empty file in order to have a valid file argument when
          ;; we reparent the root file system.  Read on for why that's
          ;; necessary.
          (with-output-to-file ".empty" (lambda () (display "")))

          ;; Create the squashfs image in several steps.
          ;; Add all store items.  Unfortunately mksquashfs throws away all
          ;; ancestor directories and only keeps the basename.  We fix this
          ;; in the following invocations of mksquashfs.
          (apply invoke "mksquashfs"
                 `(,@(map store-info-item
                          (call-with-input-file "profile"
                            read-reference-graph))
                   ,#$output

                   ;; Do not perform duplicate checking because we
                   ;; don't have any dupes.
                   "-no-duplicates"
                   "-comp"
                   ,#+(compressor-name compressor)))

          ;; Here we reparent the store items.  For each sub-directory of
          ;; the store prefix we need one invocation of "mksquashfs".
          (for-each (lambda (dir)
                      (apply invoke "mksquashfs"
                             `(".empty"
                               ,#$output
                               "-root-becomes" ,dir)))
                    (reverse (string-tokenize (%store-directory)
                                              (char-set-complement (char-set #\/)))))

          ;; Add symlinks and mount points.
          (apply invoke "mksquashfs"
                 `(".empty"
                   ,#$output
                   ;; Create SYMLINKS via pseudo file definitions.
                   ,@(append-map
                      (match-lambda
                        ((source '-> target)
                         ;; Create relative symlinks to work around a bug in
                         ;; Singularity 2.x:
                         ;;   https://bugs.gnu.org/34913
                         ;;   https://github.com/sylabs/singularity/issues/1487
                         (let ((target (string-append #$profile "/" target)))
                           (list "-p"
                                 (string-join
                                  ;; name s mode uid gid symlink
                                  (list source
                                        "s" "777" "0" "0"
                                        (relative-file-name (dirname source)
                                                            target)))))))
                      '#$symlinks)

                   ;; Create empty mount points.
                   "-p" "/proc d 555 0 0"
                   "-p" "/sys d 555 0 0"
                   "-p" "/dev d 555 0 0"))

          (when database
            ;; Initialize /var/guix.
            (install-database-and-gc-roots "var-etc" database #$profile)
            (invoke "mksquashfs" "var-etc" #$output)))))

  (gexp->derivation (string-append name
                                   (compressor-extension compressor)
                                   ".squashfs")
                    build
                    #:references-graphs `(("profile" ,profile))))

(define* (docker-image name profile
                       #:key target
                       (profile-name "guix-profile")
                       (compressor (first %compressors))
                       localstatedir?
                       (symlinks '())
                       (archiver tar))
  "Return a derivation to construct a Docker image of PROFILE.  The
image is a tarball conforming to the Docker Image Specification, compressed
with COMPRESSOR.  It can be passed to 'docker load'.  If TARGET is true, it
must a be a GNU triplet and it is used to derive the architecture metadata in
the image."
  (define database
    (and localstatedir?
         (file-append (store-database (list profile))
                      "/db/db.sqlite")))

  (define defmod 'define-module)                  ;trick Geiser

  (define build
    ;; Guile-JSON and Guile-Gcrypt are required by (guix docker).
    (with-extensions (list guile-json guile-gcrypt)
      (with-imported-modules (source-module-closure '((guix docker)
                                                      (guix build store-copy))
                                                    #:select? not-config?)
        #~(begin
            (use-modules (guix docker) (srfi srfi-19) (guix build store-copy))

            (setenv "PATH" (string-append #$archiver "/bin"))

            (build-docker-image #$output
                                (map store-info-item
                                     (call-with-input-file "profile"
                                       read-reference-graph))
                                #$profile
                                #:database #+database
                                #:system (or #$target (utsname:machine (uname)))
                                #:symlinks '#$symlinks
                                #:compressor '#$(compressor-command compressor)
                                #:creation-time (make-time time-utc 0 1))))))

  (gexp->derivation (string-append name ".tar"
                                   (compressor-extension compressor))
                    build
                    #:references-graphs `(("profile" ,profile))))


;;;
;;; Compiling C programs.
;;;

;; A C compiler.  That lowers to a single program that can be passed typical C
;; compiler flags, and it makes sure the whole toolchain is available.
(define-record-type <c-compiler>
  (%c-compiler toolchain guile)
  c-compiler?
  (toolchain c-compiler-toolchain)
  (guile     c-compiler-guile))

(define* (c-compiler #:optional inputs
                     #:key (guile (default-guile)))
  (%c-compiler inputs guile))

(define (bootstrap-c-compiler)
  "Return the C compiler that uses the bootstrap toolchain.  This is used only
by '--bootstrap', for testing purposes."
  (define bootstrap-toolchain
    (list (first (assoc-ref %bootstrap-inputs "gcc"))
          (first (assoc-ref %bootstrap-inputs "binutils"))
          (first (assoc-ref %bootstrap-inputs "libc"))))

  (c-compiler bootstrap-toolchain
              #:guile %bootstrap-guile))

(define-gexp-compiler (c-compiler-compiler (compiler <c-compiler>) system target)
  "Lower COMPILER to a single script that does the right thing."
  (define toolchain
    (or (c-compiler-toolchain compiler)
        (list (first (assoc-ref (standard-packages) "gcc"))
              (first (assoc-ref (standard-packages) "ld-wrapper"))
              (first (assoc-ref (standard-packages) "binutils"))
              (first (assoc-ref (standard-packages) "libc"))
              (gexp-input (first (assoc-ref (standard-packages) "libc"))
                          "static"))))

  (define inputs
    (match (append-map package-propagated-inputs
                       (filter package? toolchain))
      (((labels things . _) ...)
       (append toolchain things))))

  (define search-paths
    (cons $PATH
          (append-map package-native-search-paths
                      (filter package? inputs))))

  (define run
    (with-imported-modules (source-module-closure
                            '((guix build utils)
                              (guix search-paths)))
      #~(begin
          (use-modules (guix build utils) (guix search-paths)
                       (ice-9 match))

          (define (output-file args)
            (let loop ((args args))
              (match args
                (() "a.out")
                (("-o" file _ ...) file)
                ((head rest ...) (loop rest)))))

          (set-search-paths (map sexp->search-path-specification
                                 '#$(map search-path-specification->sexp
                                         search-paths))
                            '#$inputs)

          (let ((output (output-file (command-line))))
            (apply invoke "gcc" (cdr (command-line)))
            (invoke "strip" output)))))

  (when target
    ;; TODO: Yep, we'll have to do it someday!
    (leave (G_ "cross-compilation not implemented here;
please email '~a'~%")
           (@ (guix config) %guix-bug-report-address)))

  (gexp->script "c-compiler" run
                #:guile (c-compiler-guile compiler)))


;;;
;;; Wrapped package.
;;;

(define* (wrapped-package package
                          #:optional (compiler (c-compiler))
                          #:key proot?)
  (define runner
    (local-file (search-auxiliary-file "run-in-namespace.c")))

  (define (proot)
    (specification->package "proot-static"))

  (define build
    (with-imported-modules (source-module-closure
                            '((guix build utils)
                              (guix build union)))
      #~(begin
          (use-modules (guix build utils)
                       ((guix build union) #:select (relative-file-name))
                       (ice-9 ftw)
                       (ice-9 match))

          (define (strip-store-prefix file)
            ;; Given a file name like "/gnu/store/…-foo-1.2/bin/foo", return
            ;; "/bin/foo".
            (let* ((len  (string-length (%store-directory)))
                   (base (string-drop file (+ 1 len))))
              (match (string-index base #\/)
                (#f    base)
                (index (string-drop base index)))))

          (define (build-wrapper program)
            ;; Build a user-namespace wrapper for PROGRAM.
            (format #t "building wrapper for '~a'...~%" program)
            (copy-file #$runner "run.c")

            (substitute* "run.c"
              (("@WRAPPED_PROGRAM@") program)
              (("@STORE_DIRECTORY@") (%store-directory)))

            (let* ((base   (strip-store-prefix program))
                   (result (string-append #$output "/" base))
                   (proot  #$(and proot?
                                  #~(string-drop
                                     #$(file-append (proot) "/bin/proot")
                                     (+ (string-length (%store-directory))
                                        1)))))
              (mkdir-p (dirname result))
              (apply invoke #$compiler "-std=gnu99" "-static" "-Os" "-g0" "-Wall"
                     "run.c" "-o" result
                     (if proot
                         (list (string-append "-DPROOT_PROGRAM=\""
                                              proot "\""))
                         '()))
              (delete-file "run.c")))

          (setvbuf (current-output-port) 'line)

          ;; Link the top-level files of PACKAGE so that search paths are
          ;; properly defined in PROFILE/etc/profile.
          (mkdir #$output)
          (for-each (lambda (file)
                      (unless (member file '("." ".." "bin" "sbin" "libexec"))
                        (let ((file* (string-append #$package "/" file)))
                          (symlink (relative-file-name #$output file*)
                                   (string-append #$output "/" file)))))
                    (scandir #$package))

          (for-each build-wrapper
                    (append (find-files #$(file-append package "/bin"))
                            (find-files #$(file-append package "/sbin"))
                            (find-files #$(file-append package "/libexec")))))))

  (computed-file (string-append
                  (cond ((package? package)
                         (package-full-name package "-"))
                        ((inferior-package? package)
                         (string-append (inferior-package-name package)
                                        "-"
                                        (inferior-package-version package)))
                        (else "wrapper"))
                  "R")
                 build))

(define (map-manifest-entries proc manifest)
  "Apply PROC to all the entries of MANIFEST and return a new manifest."
  (make-manifest
   (map (lambda (entry)
          (manifest-entry
            (inherit entry)
            (item (proc (manifest-entry-item entry)))))
        (manifest-entries manifest))))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  `((format . tarball)
    (profile-name . "guix-profile")
    (system . ,(%current-system))
    (substitutes? . #t)
    (build-hook? . #t)
    (graft? . #t)
    (print-build-trace? . #t)
    (print-extended-build-trace? . #t)
    (multiplexed-build-output? . #t)
    (debug . 0)
    (verbosity . 2)
    (symlinks . ())
    (compressor . ,(first %compressors))))

(define %formats
  ;; Supported pack formats.
  `((tarball . ,self-contained-tarball)
    (squashfs . ,squashfs-image)
    (docker  . ,docker-image)))

(define (show-formats)
  ;; Print the supported pack formats.
  (display (G_ "The supported formats for 'guix pack' are:"))
  (newline)
  (display (G_ "
  tarball       Self-contained tarball, ready to run on another machine"))
  (display (G_ "
  squashfs      Squashfs image suitable for Singularity"))
  (display (G_ "
  docker        Tarball ready for 'docker load'"))
  (newline))

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
         (option '(#\f "format") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'format (string->symbol arg) result)))
         (option '("list-formats") #f #f
                 (lambda args
                   (show-formats)
                   (exit 0)))
         (option '(#\R "relocatable") #f #f
                 (lambda (opt name arg result)
                   (match (assq-ref result 'relocatable?)
                     (#f
                      (alist-cons 'relocatable? #t result))
                     (_
                      (alist-cons 'relocatable? 'proot
                                  (alist-delete 'relocatable? result))))))
         (option '(#\e "expression") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'expression arg result)))
         (option '(#\m "manifest") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'manifest arg result)))
         (option '(#\s "system") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'system arg
                               (alist-delete 'system result eq?))))
         (option '("target") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'target arg
                               (alist-delete 'target result eq?))))
         (option '(#\C "compression") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'compressor (lookup-compressor arg)
                               result)))
         (option '(#\S "symlink") #t #f
                 (lambda (opt name arg result)
                   ;; Note: Using 'string-split' allows us to handle empty
                   ;; TARGET (as in "/opt/guile=", meaning that /opt/guile is
                   ;; a symlink to the profile) correctly.
                   (match (string-split arg (char-set #\=))
                     ((source target)
                      (let ((symlinks (assoc-ref result 'symlinks)))
                        (alist-cons 'symlinks
                                    `((,source -> ,target) ,@symlinks)
                                    (alist-delete 'symlinks result eq?))))
                     (x
                      (leave (G_ "~a: invalid symlink specification~%")
                             arg)))))
         (option '("save-provenance") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'save-provenance? #t result)))
         (option '("localstatedir") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'localstatedir? #t result)))
         (option '("profile-name") #t #f
                 (lambda (opt name arg result)
                   (match arg
                     ((or "guix-profile" "current-guix")
                      (alist-cons 'profile-name arg result))
                     (_
                      (leave (G_ "~a: unsupported profile name~%") arg)))))
         (option '(#\v "verbosity") #t #f
                 (lambda (opt name arg result)
                   (let ((level (string->number* arg)))
                     (alist-cons 'verbosity level
                                 (alist-delete 'verbosity result)))))
         (option '("bootstrap") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'bootstrap? #t result)))

         (append %transformation-options
                 %standard-build-options)))

(define (show-help)
  (display (G_ "Usage: guix pack [OPTION]... PACKAGE...
Create a bundle of PACKAGE.\n"))
  (show-build-options-help)
  (newline)
  (show-transformation-options-help)
  (newline)
  (display (G_ "
  -f, --format=FORMAT    build a pack in the given FORMAT"))
  (display (G_ "
      --list-formats     list the formats available"))
  (display (G_ "
  -R, --relocatable      produce relocatable executables"))
  (display (G_ "
  -e, --expression=EXPR  consider the package EXPR evaluates to"))
  (display (G_ "
  -s, --system=SYSTEM    attempt to build for SYSTEM--e.g., \"i686-linux\""))
  (display (G_ "
      --target=TRIPLET   cross-build for TRIPLET--e.g., \"armel-linux-gnu\""))
  (display (G_ "
  -C, --compression=TOOL compress using TOOL--e.g., \"lzip\""))
  (display (G_ "
  -S, --symlink=SPEC     create symlinks to the profile according to SPEC"))
  (display (G_ "
  -m, --manifest=FILE    create a pack with the manifest from FILE"))
  (display (G_ "
      --save-provenance  save provenance information"))
  (display (G_ "
      --localstatedir    include /var/guix in the resulting pack"))
  (display (G_ "
      --profile-name=NAME
                         populate /var/guix/profiles/.../NAME"))
  (display (G_ "
  -v, --verbosity=LEVEL  use the given verbosity LEVEL"))
  (display (G_ "
      --bootstrap        use the bootstrap binaries to build the pack"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))


;;;
;;; Entry point.
;;;

(define (guix-pack . args)
  (define opts
    (parse-command-line args %options (list %default-options)))

  (define maybe-package-argument
    ;; Given an option pair, return a package, a package/output tuple, or #f.
    (match-lambda
      (('argument . spec)
       (call-with-values
           (lambda ()
             (specification->package+output spec))
         list))
      (('expression . exp)
       (read/eval-package-expression exp))
      (x #f)))

  (define (manifest-from-args store opts)
    (let* ((transform     (options->transformation opts))
           (packages      (map (match-lambda
                                 (((? package? package) output)
                                  (list (transform store package) output))
                                 ((? package? package)
                                  (list (transform store package) "out")))
                               (filter-map maybe-package-argument opts)))
           (manifest-file (assoc-ref opts 'manifest)))
      (define properties
        (if (assoc-ref opts 'save-provenance?)
            (lambda (package)
              (match (package-provenance package)
                (#f
                 (warning (G_ "could not determine provenance of package ~a~%")
                          (package-full-name package))
                 '())
                (sexp
                 `((provenance . ,sexp)))))
            (const '())))

      (cond
       ((and manifest-file (not (null? packages)))
        (leave (G_ "both a manifest and a package list were given~%")))
       (manifest-file
        (let ((user-module (make-user-module '((guix profiles) (gnu)))))
          (load* manifest-file user-module)))
       (else
        (manifest
         (map (match-lambda
                ((package output)
                 (package->manifest-entry package output
                                          #:properties
                                          (properties package))))
              packages))))))

  (with-error-handling
    (with-store store
      (with-status-verbosity (assoc-ref opts 'verbosity)
        ;; Set the build options before we do anything else.
        (set-build-options-from-command-line store opts)

        (parameterize ((%graft? (assoc-ref opts 'graft?))
                       (%guile-for-build (package-derivation
                                          store
                                          (if (assoc-ref opts 'bootstrap?)
                                              %bootstrap-guile
                                              (canonical-package guile-2.2))
                                          (assoc-ref opts 'system)
                                          #:graft? (assoc-ref opts 'graft?))))
          (let* ((dry-run?    (assoc-ref opts 'dry-run?))
                 (relocatable? (assoc-ref opts 'relocatable?))
                 (proot?      (eq? relocatable? 'proot))
                 (manifest    (let ((manifest (manifest-from-args store opts)))
                                ;; Note: We cannot honor '--bootstrap' here because
                                ;; 'glibc-bootstrap' lacks 'libc.a'.
                                (if relocatable?
                                    (map-manifest-entries
                                     (cut wrapped-package <> #:proot? proot?)
                                     manifest)
                                    manifest)))
                 (pack-format (assoc-ref opts 'format))
                 (name        (string-append (symbol->string pack-format)
                                             "-pack"))
                 (target      (assoc-ref opts 'target))
                 (bootstrap?  (assoc-ref opts 'bootstrap?))
                 (compressor  (if bootstrap?
                                  bootstrap-xz
                                  (assoc-ref opts 'compressor)))
                 (archiver    (if (equal? pack-format 'squashfs)
                                  squashfs-tools-next
                                  (if bootstrap?
                                      %bootstrap-coreutils&co
                                      tar)))
                 (symlinks    (assoc-ref opts 'symlinks))
                 (build-image (match (assq-ref %formats pack-format)
                                ((? procedure? proc) proc)
                                (#f
                                 (leave (G_ "~a: unknown pack format~%")
                                        pack-format))))
                 (localstatedir? (assoc-ref opts 'localstatedir?))
                 (profile-name   (assoc-ref opts 'profile-name)))
            (run-with-store store
              (mlet* %store-monad ((profile (profile-derivation
                                             manifest

                                             ;; Always produce relative
                                             ;; symlinks for Singularity (see
                                             ;; <https://bugs.gnu.org/34913>).
                                             #:relative-symlinks?
                                             (or relocatable?
                                                 (eq? 'squashfs pack-format))

                                             #:hooks (if bootstrap?
                                                         '()
                                                         %default-profile-hooks)
                                             #:locales? (not bootstrap?)
                                             #:target target))
                                   (drv (build-image name profile
                                                     #:target
                                                     target
                                                     #:compressor
                                                     compressor
                                                     #:symlinks
                                                     symlinks
                                                     #:localstatedir?
                                                     localstatedir?
                                                     #:profile-name
                                                     profile-name
                                                     #:archiver
                                                     archiver)))
                (mbegin %store-monad
                  (show-what-to-build* (list drv)
                                       #:use-substitutes?
                                       (assoc-ref opts 'substitutes?)
                                       #:dry-run? dry-run?)
                  (munless dry-run?
                    (built-derivations (list drv))
                    (return (format #t "~a~%"
                                    (derivation->output-path drv))))))
              #:system (assoc-ref opts 'system))))))))
