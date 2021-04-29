;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2015, 2018, 2021 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Brendan Tildesley <mail@brendan.scot>
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

(define-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-60)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (ice-9 threads)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:re-export (alist-cons
               alist-delete

               ;; Note: Re-export 'delete' to allow for proper syntax matching
               ;; in 'modify-phases' forms.  See
               ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26805#16>.
               delete)
  #:export (%store-directory
            store-file-name?
            strip-store-file-name
            package-name->name+version
            parallel-job-count

            compressor
            tarball?
            %xz-parallel-args

            directory-exists?
            executable-file?
            symbolic-link?
            call-with-temporary-output-file
            call-with-ascii-input-file
            elf-file?
            ar-file?
            gzip-file?
            reset-gzip-timestamp
            with-directory-excursion
            mkdir-p
            install-file
            make-file-writable
            copy-recursively
            delete-file-recursively
            file-name-predicate
            find-files
            false-if-file-not-found

            search-path-as-list
            set-path-environment-variable
            search-path-as-string->list
            list->search-path-as-string
            which
            search-input-file
            search-input-directory
            search-error?
            search-error-path
            search-error-file

            every*
            alist-cons-before
            alist-cons-after
            alist-replace
            modify-phases

            with-atomic-file-replacement
            substitute
            substitute*
            dump-port
            set-file-time
            patch-shebang
            patch-makefile-SHELL
            patch-/usr/bin/file
            fold-port-matches
            remove-store-references
            wrapped-program?
            wrap-program
            wrap-script

            wrap-error?
            wrap-error-program
            wrap-error-type

            invoke
            invoke-error?
            invoke-error-program
            invoke-error-arguments
            invoke-error-exit-status
            invoke-error-term-signal
            invoke-error-stop-signal
            report-invoke-error

            invoke/quiet

            make-desktop-entry-file

            locale-category->string))


;;;
;;; Guile 2.0 compatibility later.
;;;

;; The bootstrap Guile is Guile 2.0, so provide a compatibility layer.
(cond-expand
  ((and guile-2 (not guile-2.2))
   (define (setvbuf port mode . rest)
     (apply (@ (guile) setvbuf) port
            (match mode
              ('line _IOLBF)
              ('block _IOFBF)
              ('none _IONBF)
              (_ mode))                           ;an _IO* integer
            rest))

   (module-replace! (current-module) '(setvbuf)))
  (else #f))


;;;
;;; Compression helpers.
;;;

(define (compressor file-name)
  "Return the name of the compressor package/binary used to compress or
decompress FILE-NAME, based on its file extension, else false."
  (cond ((string-suffix? "gz"  file-name)  "gzip")
        ((string-suffix? "Z"   file-name)  "gzip")
        ((string-suffix? "bz2" file-name)  "bzip2")
        ((string-suffix? "lz"  file-name)  "lzip")
        ((string-suffix? "zip" file-name)  "unzip")
        ((string-suffix? "xz"  file-name)  "xz")
        (else #f)))                ;no compression used/unknown file extension

(define (tarball? file-name)
  "True when FILE-NAME has a tar file extension."
  (string-match "\\.(tar(\\..*)?|tgz|tbz)$" file-name))

(define (%xz-parallel-args)
  "The xz arguments required to enable bit-reproducible, multi-threaded
compression."
  (list "--memlimit=50%"
        (format #f "--threads=~a" (max 2 (parallel-job-count)))))


;;;
;;; Directories.
;;;

(define (%store-directory)
  "Return the directory name of the store."
  (or (getenv "NIX_STORE_DIR")          ;outside of builder
      (getenv "NIX_STORE")              ;inside builder, set by the daemon
      "/gnu/store"))

(define (store-file-name? file)
  "Return true if FILE is in the store."
  (string-prefix? (%store-directory) file))

(define (strip-store-file-name file)
  "Strip the '/gnu/store' and hash from FILE, a store file name.  The result
is typically a \"PACKAGE-VERSION\" string."
  (string-drop file
               (+ 34 (string-length (%store-directory)))))

(define (package-name->name+version name)
  "Given NAME, a package name like \"foo-0.9.1b\", return two values:
\"foo\" and \"0.9.1b\".  When the version part is unavailable, NAME and
#f are returned.  The first hyphen followed by a digit is considered to
introduce the version part."
  ;; See also `DrvName' in Nix.

  (define number?
    (cut char-set-contains? char-set:digit <>))

  (let loop ((chars   (string->list name))
             (prefix '()))
    (match chars
      (()
       (values name #f))
      ((#\- (? number? n) rest ...)
       (values (list->string (reverse prefix))
               (list->string (cons n rest))))
      ((head tail ...)
       (loop tail (cons head prefix))))))

(define parallel-job-count
  ;; Number of processes to be passed next to GNU Make's `-j' argument.
  (make-parameter
   (match (getenv "NIX_BUILD_CORES")              ;set by the daemon
     (#f  1)
     ("0" (current-processor-count))
     (x   (or (string->number x) 1)))))

(define (directory-exists? dir)
  "Return #t if DIR exists and is a directory."
  (let ((s (stat dir #f)))
    (and s
         (eq? 'directory (stat:type s)))))

(define (executable-file? file)
  "Return #t if FILE exists and is executable."
  (let ((s (stat file #f)))
    (and s
         (not (zero? (logand (stat:mode s) #o100))))))

(define (symbolic-link? file)
  "Return #t if FILE is a symbolic link (aka. \"symlink\".)"
  (eq? (stat:type (lstat file)) 'symlink))

(define (call-with-temporary-output-file proc)
  "Call PROC with a name of a temporary file and open output port to that
file; close the file and delete it when leaving the dynamic extent of this
call."
  (let* ((directory (or (getenv "TMPDIR") "/tmp"))
         (template  (string-append directory "/guix-file.XXXXXX"))
         (out       (mkstemp! template)))
    (dynamic-wind
      (lambda ()
        #t)
      (lambda ()
        (proc template out))
      (lambda ()
        (false-if-exception (close out))
        (false-if-exception (delete-file template))))))

(define (call-with-ascii-input-file file proc)
  "Open FILE as an ASCII or binary file, and pass the resulting port to
PROC.  FILE is closed when PROC's dynamic extent is left.  Return the
return values of applying PROC to the port."
  (let ((port (with-fluids ((%default-port-encoding #f))
                ;; Use "b" so that `open-file' ignores `coding:' cookies.
                (open-file file "rb"))))
    (dynamic-wind
      (lambda ()
        #t)
      (lambda ()
        (proc port))
      (lambda ()
        (close-input-port port)))))

(define (file-header-match header)
  "Return a procedure that returns true when its argument is a file starting
with the bytes in HEADER, a bytevector."
  (define len
    (bytevector-length header))

  (lambda (file)
    "Return true if FILE starts with the right magic bytes."
    (define (get-header)
      (call-with-input-file file
        (lambda (port)
          (get-bytevector-n port len))
        #:binary #t #:guess-encoding #f))

    (catch 'system-error
      (lambda ()
        (equal? (get-header) header))
      (lambda args
        (if (= EISDIR (system-error-errno args))
            #f                                    ;FILE is a directory
            (apply throw args))))))

(define %elf-magic-bytes
  ;; Magic bytes of ELF files.  See <elf.h>.
  (u8-list->bytevector (map char->integer (string->list "\x7FELF"))))

(define elf-file?
  (file-header-match %elf-magic-bytes))

(define %ar-magic-bytes
  ;; Magic bytes of archives created by 'ar'.  See <ar.h>.
  (u8-list->bytevector (map char->integer (string->list "!<arch>\n"))))

(define ar-file?
  (file-header-match %ar-magic-bytes))

(define %gzip-magic-bytes
  ;; Magic bytes of gzip file.  Beware, it's a small header so there could be
  ;; false positives.
  #vu8(#x1f #x8b))

(define gzip-file?
  (file-header-match %gzip-magic-bytes))

(define* (reset-gzip-timestamp file #:key (keep-mtime? #t))
  "If FILE is a gzip file, reset its embedded timestamp (as with 'gzip
--no-name') and return true.  Otherwise return #f.  When KEEP-MTIME? is true,
preserve FILE's modification time."
  (let ((stat (stat file))
        (port (open file O_RDWR)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (and (= 4 (seek port 4 SEEK_SET))
             (put-bytevector port #vu8(0 0 0 0))))
      (lambda ()
        (close-port port)
        (set-file-time file stat)))))

(define-syntax-rule (with-directory-excursion dir body ...)
  "Run BODY with DIR as the process's current directory."
  (let ((init (getcwd)))
   (dynamic-wind
     (lambda ()
       (chdir dir))
     (lambda ()
       body ...)
     (lambda ()
       (chdir init)))))

(define (mkdir-p dir)
  "Create directory DIR and all its ancestors."
  (define absolute?
    (string-prefix? "/" dir))

  (define not-slash
    (char-set-complement (char-set #\/)))

  (let loop ((components (string-tokenize dir not-slash))
             (root       (if absolute?
                             ""
                             ".")))
    (match components
      ((head tail ...)
       (let ((path (string-append root "/" head)))
         (catch 'system-error
           (lambda ()
             (mkdir path)
             (loop tail path))
           (lambda args
             (if (= EEXIST (system-error-errno args))
                 (loop tail path)
                 (apply throw args))))))
      (() #t))))

(define (install-file file directory)
  "Create DIRECTORY if it does not exist and copy FILE in there under the same
name."
  (mkdir-p directory)
  (copy-file file (string-append directory "/" (basename file))))

(define (make-file-writable file)
  "Make FILE writable for its owner."
  (let ((stat (lstat file)))                      ;XXX: symlinks
    (chmod file (logior #o600 (stat:perms stat)))))

(define* (copy-recursively source destination
                           #:key
                           (log (current-output-port))
                           (follow-symlinks? #f)
                           (copy-file copy-file)
                           keep-mtime? keep-permissions?)
  "Copy SOURCE directory to DESTINATION.  Follow symlinks if FOLLOW-SYMLINKS?
is true; otherwise, just preserve them.  Call COPY-FILE to copy regular files.
When KEEP-MTIME? is true, keep the modification time of the files in SOURCE on
those of DESTINATION.  When KEEP-PERMISSIONS? is true, preserve file
permissions.  Write verbose output to the LOG port."
  (define strip-source
    (let ((len (string-length source)))
      (lambda (file)
        (substring file len))))

  (file-system-fold (const #t)                    ; enter?
                    (lambda (file stat result)    ; leaf
                      (let ((dest (string-append destination
                                                 (strip-source file))))
                        (format log "`~a' -> `~a'~%" file dest)
                        (case (stat:type stat)
                          ((symlink)
                           (let ((target (readlink file)))
                             (symlink target dest)))
                          (else
                           (copy-file file dest)
                           (when keep-permissions?
                             (chmod dest (stat:perms stat)))))
                        (when keep-mtime?
                          (set-file-time dest stat))))
                    (lambda (dir stat result)     ; down
                      (let ((target (string-append destination
                                                   (strip-source dir))))
                        (mkdir-p target)))
                    (lambda (dir stat result)     ; up
                      (let ((target (string-append destination
                                                   (strip-source dir))))
                        (when keep-mtime?
                          (set-file-time target stat))
                        (when keep-permissions?
                          (chmod target (stat:perms stat)))))
                    (const #t)                    ; skip
                    (lambda (file stat errno result)
                      (format (current-error-port) "i/o error: ~a: ~a~%"
                              file (strerror errno))
                      #f)
                    #t
                    source

                    (if follow-symlinks?
                        stat
                        lstat)))

(define-syntax-rule (warn-on-error expr file)
  (catch 'system-error
    (lambda ()
      expr)
    (lambda args
      (format (current-error-port)
              "warning: failed to delete ~a: ~a~%"
              file (strerror
                    (system-error-errno args))))))

(define* (delete-file-recursively dir
                                  #:key follow-mounts?)
  "Delete DIR recursively, like `rm -rf', without following symlinks.  Don't
follow mount points either, unless FOLLOW-MOUNTS? is true.  Report but ignore
errors."
  (let ((dev (stat:dev (lstat dir))))
    (file-system-fold (lambda (dir stat result)    ; enter?
                        (or follow-mounts?
                            (= dev (stat:dev stat))))
                      (lambda (file stat result)   ; leaf
                        (warn-on-error (delete-file file) file))
                      (const #t)                   ; down
                      (lambda (dir stat result)    ; up
                        (warn-on-error (rmdir dir) dir))
                      (const #t)                   ; skip
                      (lambda (file stat errno result)
                        (format (current-error-port)
                                "warning: failed to delete ~a: ~a~%"
                                file (strerror errno)))
                      #t
                      dir

                      ;; Don't follow symlinks.
                      lstat)))

(define (file-name-predicate regexp)
  "Return a predicate that returns true when passed a file name whose base
name matches REGEXP."
  (let ((file-rx (if (regexp? regexp)
                     regexp
                     (make-regexp regexp))))
    (lambda (file stat)
      (regexp-exec file-rx (basename file)))))

(define* (find-files dir #:optional (pred (const #t))
                     #:key (stat lstat)
                     directories?
                     fail-on-error?)
  "Return the lexicographically sorted list of files under DIR for which PRED
returns true.  PRED is passed two arguments: the absolute file name, and its
stat buffer; the default predicate always returns true.  PRED can also be a
regular expression, in which case it is equivalent to (file-name-predicate
PRED).  STAT is used to obtain file information; using 'lstat' means that
symlinks are not followed.  If DIRECTORIES? is true, then directories will
also be included.  If FAIL-ON-ERROR? is true, raise an exception upon error."
  (let ((pred (if (procedure? pred)
                  pred
                  (file-name-predicate pred))))
    ;; Sort the result to get deterministic results.
    (sort (file-system-fold (const #t)
                            (lambda (file stat result) ; leaf
                              (if (pred file stat)
                                  (cons file result)
                                  result))
                            (lambda (dir stat result) ; down
                              (if (and directories?
                                       (pred dir stat))
                                  (cons dir result)
                                  result))
                            (lambda (dir stat result) ; up
                              result)
                            (lambda (file stat result) ; skip
                              result)
                            (lambda (file stat errno result)
                              (format (current-error-port) "find-files: ~a: ~a~%"
                                      file (strerror errno))
                              (when fail-on-error?
                                (error "find-files failed"))
                              result)
                            '()
                            dir
                            stat)
          string<?)))

(define-syntax-rule (false-if-file-not-found exp)
  "Evaluate EXP but return #f if it raises to 'system-error with ENOENT."
  (catch 'system-error
    (lambda () exp)
    (lambda args
      (if (= ENOENT (system-error-errno args))
          #f
          (apply throw args)))))


;;;
;;; Search paths.
;;;

(define* (search-path-as-list files input-dirs
                              #:key (type 'directory) pattern)
  "Return the list of directories among FILES of the given TYPE (a symbol as
returned by 'stat:type') that exist in INPUT-DIRS.  Example:

  (search-path-as-list '(\"share/emacs/site-lisp\" \"share/emacs/24.1\")
                       (list \"/package1\" \"/package2\" \"/package3\"))
  => (\"/package1/share/emacs/site-lisp\"
      \"/package3/share/emacs/site-lisp\")

When PATTERN is true, it is a regular expression denoting file names to look
for under the directories designated by FILES.  For example:

  (search-path-as-list '(\"xml\") (list docbook-xml docbook-xsl)
                       #:type 'regular
                       #:pattern \"^catalog\\\\.xml$\")
  => (\"/…/xml/dtd/docbook/catalog.xml\"
      \"/…/xml/xsl/docbook-xsl-1.78.1/catalog.xml\")
"
  (append-map (lambda (input)
                (append-map (lambda (file)
                              (let ((file (string-append input "/" file)))
                                (if pattern
                                    (find-files file (lambda (file stat)
                                                       (and stat
                                                            (eq? type (stat:type stat))
                                                            ((file-name-predicate pattern) file stat)))
                                                #:stat stat
                                                #:directories? #t)
                                    (let ((stat (stat file #f)))
                                      (if (and stat (eq? type (stat:type stat)))
                                          (list file)
                                          '())))))
                            files))
              (delete-duplicates input-dirs)))

(define (list->search-path-as-string lst separator)
  (if separator
      (string-join lst separator)
      (match lst
        ((head rest ...) head)
        (() ""))))

(define* (search-path-as-string->list path #:optional (separator #\:))
  (if separator
      (string-tokenize path
                       (char-set-complement (char-set separator)))
      (list path)))

(define* (set-path-environment-variable env-var files input-dirs
                                        #:key
                                        (separator ":")
                                        (type 'directory)
                                        pattern)
  "Look for each of FILES of the given TYPE (a symbol as returned by
'stat:type') in INPUT-DIRS.  Set ENV-VAR to a SEPARATOR-separated path
accordingly.  Example:

  (set-path-environment-variable \"PKG_CONFIG\"
                                 '(\"lib/pkgconfig\")
                                 (list package1 package2))

When PATTERN is not #f, it must be a regular expression (really a string)
denoting file names to look for under the directories designated by FILES:

  (set-path-environment-variable \"XML_CATALOG_FILES\"
                                 '(\"xml\")
                                 (list docbook-xml docbook-xsl)
                                 #:type 'regular
                                 #:pattern \"^catalog\\\\.xml$\")
"
  (let* ((path  (search-path-as-list files input-dirs
                                     #:type type
                                     #:pattern pattern))
         (value (list->search-path-as-string path separator)))
    (if (string-null? value)
        (begin
          ;; Never set ENV-VAR to an empty string because often, the empty
          ;; string is equivalent to ".".  This is the case for
          ;; GUILE_LOAD_PATH in Guile 2.0, for instance.
          (unsetenv env-var)
          (format #t "environment variable `~a' unset~%" env-var))
        (begin
          (setenv env-var value)
          (format #t "environment variable `~a' set to `~a'~%"
                  env-var value)))))

(define (which program)
  "Return the complete file name for PROGRAM as found in $PATH, or #f if
PROGRAM could not be found."
  (search-path (search-path-as-string->list (getenv "PATH"))
               program))

(define-condition-type &search-error &error
  search-error?
  (path         search-error-path)
  (file         search-error-file))

(define (search-input-file inputs file)
  "Find a file named FILE among the INPUTS and return its absolute file name.

FILE must be a string like \"bin/sh\". If FILE is not found, an exception is
raised."
  (match inputs
    (((_ . directories) ...)
     ;; Accept both "bin/sh" and "/bin/sh" as FILE argument.
     (let ((file (string-trim file #\/)))
       (or (search-path directories file)
           (raise
            (condition (&search-error (path directories) (file file)))))))))

(define (search-input-directory inputs directory)
  "Find a sub-directory named DIRECTORY among the INPUTS and return its
absolute file name.

DIRECTORY must be a string like \"xml/dtd/docbook\".  If DIRECTORY is not
found, an exception is raised."
  (match inputs
    (((_ . directories) ...)
     (or (any (lambda (parent)
                (let ((directory (string-append parent "/" directory)))
                  (and (directory-exists? directory)
                       directory)))
              directories)
         (raise (condition
                 (&search-error (path directories) (file directory))))))))


;;;
;;; Phases.
;;;
;;; In (guix build gnu-build-system), there are separate phases (configure,
;;; build, test, install).  They are represented as a list of name/procedure
;;; pairs.  The following procedures make it easy to change the list of
;;; phases.
;;;

(define (every* pred lst)
  "This is like 'every', but process all the elements of LST instead of
stopping as soon as PRED returns false.  This is useful when PRED has side
effects, such as displaying warnings or error messages."
  (let loop ((lst    lst)
             (result #t))
    (match lst
      (()
       result)
      ((head . tail)
       (loop tail (and (pred head) result))))))

(define* (alist-cons-before reference key value alist
                            #:optional (key=? equal?))
  "Insert the KEY/VALUE pair before the first occurrence of a pair whose key
is REFERENCE in ALIST.  Use KEY=? to compare keys."
  (let-values (((before after)
                (break (match-lambda
                        ((k . _)
                         (key=? k reference)))
                       alist)))
    (append before (alist-cons key value after))))

(define* (alist-cons-after reference key value alist
                           #:optional (key=? equal?))
  "Insert the KEY/VALUE pair after the first occurrence of a pair whose key
is REFERENCE in ALIST.  Use KEY=? to compare keys."
  (let-values (((before after)
                (break (match-lambda
                        ((k . _)
                         (key=? k reference)))
                       alist)))
    (match after
      ((reference after ...)
       (append before (cons* reference `(,key . ,value) after)))
      (()
       (append before `((,key . ,value)))))))

(define* (alist-replace key value alist #:optional (key=? equal?))
  "Replace the first pair in ALIST whose car is KEY with the KEY/VALUE pair.
An error is raised when no such pair exists."
  (let-values (((before after)
                (break (match-lambda
                        ((k . _)
                         (key=? k key)))
                       alist)))
    (match after
      ((_ after ...)
       (append before (alist-cons key value after))))))

(define-syntax-rule (modify-phases phases mod-spec ...)
  "Modify PHASES sequentially as per each MOD-SPEC, which may have one of the
following forms:

  (delete <old-phase-name>)
  (replace <old-phase-name> <new-phase>)
  (add-before <old-phase-name> <new-phase-name> <new-phase>)
  (add-after <old-phase-name> <new-phase-name> <new-phase>)

Where every <*-phase-name> is an expression evaluating to a symbol, and
<new-phase> an expression evaluating to a procedure."
  (let* ((phases* phases)
         (phases* (%modify-phases phases* mod-spec))
         ...)
    phases*))

(define-syntax %modify-phases
  (syntax-rules (delete replace add-before add-after)
    ((_ phases (delete old-phase-name))
     (alist-delete old-phase-name phases))
    ((_ phases (replace old-phase-name new-phase))
     (alist-replace old-phase-name new-phase phases))
    ((_ phases (add-before old-phase-name new-phase-name new-phase))
     (alist-cons-before old-phase-name new-phase-name new-phase phases))
    ((_ phases (add-after old-phase-name new-phase-name new-phase))
     (alist-cons-after old-phase-name new-phase-name new-phase phases))))


;;;
;;; Program invocation.
;;;

(define-condition-type &invoke-error &error
  invoke-error?
  (program      invoke-error-program)
  (arguments    invoke-error-arguments)
  (exit-status  invoke-error-exit-status)
  (term-signal  invoke-error-term-signal)
  (stop-signal  invoke-error-stop-signal))

(define (invoke program . args)
  "Invoke PROGRAM with the given ARGS.  Raise an exception
if the exit code is non-zero; otherwise return #t."
  (let ((code (apply system* program args)))
    (unless (zero? code)
      (raise (condition (&invoke-error
                         (program program)
                         (arguments args)
                         (exit-status (status:exit-val code))
                         (term-signal (status:term-sig code))
                         (stop-signal (status:stop-sig code))))))
    #t))

(define* (report-invoke-error c #:optional (port (current-error-port)))
  "Report to PORT about C, an '&invoke-error' condition, in a human-friendly
way."
  (format port "command~{ ~s~} failed with ~:[signal~;status~] ~a~%"
          (cons (invoke-error-program c)
                (invoke-error-arguments c))
          (invoke-error-exit-status c)
          (or (invoke-error-exit-status c)
              (invoke-error-term-signal c)
              (invoke-error-stop-signal c))))

(define (open-pipe-with-stderr program . args)
  "Run PROGRAM with ARGS in an input pipe, but, unlike 'open-pipe*', redirect
both its standard output and standard error to the pipe.  Return two value:
the pipe to read PROGRAM's data from, and the PID of the child process running
PROGRAM."
  ;; 'open-pipe*' doesn't attempt to capture stderr in any way, which is why
  ;; we need to roll our own.
  (match (pipe)
    ((input .  output)
     (match (primitive-fork)
       (0
        (dynamic-wind
          (const #t)
          (lambda ()
            (close-port input)
            (dup2 (fileno output) 1)
            (dup2 (fileno output) 2)
            (apply execlp program program args))
          (lambda ()
            (primitive-exit 127))))
       (pid
        (close-port output)
        (values input pid))))))

(define (invoke/quiet program . args)
  "Invoke PROGRAM with ARGS and capture PROGRAM's standard output and standard
error.  If PROGRAM succeeds, print nothing and return the unspecified value;
otherwise, raise a '&message' error condition that includes the status code
and the output of PROGRAM."
  (let-values (((pipe pid)
                (apply open-pipe-with-stderr program args)))
    (let loop ((lines '()))
      (match (read-line pipe)
        ((? eof-object?)
         (close-port pipe)
         (match (waitpid pid)
           ((_ . status)
            (unless (zero? status)
              (let-syntax ((G_ (syntax-rules ()   ;for xgettext
                                 ((_ str) str))))
                (raise (condition
                        (&message
                         (message (format #f (G_ "'~a~{ ~a~}' exited \
with status ~a; output follows:~%~%~{  ~a~%~}")
                                          program args
                                          (or (status:exit-val status)
                                              status)
                                          (reverse lines)))))))))))
        (line
         (loop (cons line lines)))))))


;;;
;;; Text substitution (aka. sed).
;;;

(define (with-atomic-file-replacement file proc)
  "Call PROC with two arguments: an input port for FILE, and an output
port for the file that is going to replace FILE.  Upon success, FILE is
atomically replaced by what has been written to the output port, and
PROC's result is returned."
  (let* ((template (string-append file ".XXXXXX"))
         (out      (mkstemp! template))
         (mode     (stat:mode (stat file))))
    (with-throw-handler #t
      (lambda ()
        (call-with-input-file file
          (lambda (in)
            (let ((result (proc in out)))
              (close out)
              (chmod template mode)
              (rename-file template file)
              result))))
      (lambda (key . args)
        (false-if-exception (delete-file template))))))

(define (unused-private-use-code-point s)
  "Find a code point within a Unicode Private Use Area that is not
present in S, and return the corresponding character object.  If one
cannot be found, return false."
  (define (scan lo hi)
    (and (<= lo hi)
         (let ((c (integer->char lo)))
           (if (string-index s c)
               (scan (+ lo 1) hi)
               c))))
  (or (scan   #xE000   #xF8FF)
      (scan  #xF0000  #xFFFFD)
      (scan #x100000 #x10FFFD)))

(define (replace-char c1 c2 s)
  "Return a string which is equal to S except with all instances of C1
replaced by C2.  If C1 and C2 are equal, return S."
  (if (char=? c1 c2)
      s
      (string-map (lambda (c)
                    (if (char=? c c1)
                        c2
                        c))
                  s)))

(define (substitute file pattern+procs)
  "PATTERN+PROCS is a list of regexp/two-argument-procedure pairs.  For each
line of FILE, and for each PATTERN that it matches, call the corresponding
PROC as (PROC LINE MATCHES); PROC must return the line that will be written as
a substitution of the original line.  Be careful about using '$' to match the
end of a line; by itself it won't match the terminating newline of a line."
  (let ((rx+proc  (map (match-lambda
                        (((? regexp? pattern) . proc)
                         (cons pattern proc))
                        ((pattern . proc)
                         (cons (make-regexp pattern regexp/extended)
                               proc)))
                       pattern+procs)))
    (with-atomic-file-replacement file
      (lambda (in out)
        (let loop ((line (read-line in 'concat)))
          (if (eof-object? line)
              #t
              ;; Work around the fact that Guile's regexp-exec does not handle
              ;; NUL characters (a limitation of the underlying GNU libc's
              ;; regexec) by temporarily replacing them by an unused private
              ;; Unicode code point.
              ;; TODO: Use SRFI-115 instead, once available in Guile.
              (let* ((nul* (or (and (string-index line #\nul)
                                    (unused-private-use-code-point line))
                               #\nul))
                     (line* (replace-char #\nul nul* line))
                     (line1* (fold (lambda (r+p line)
                                     (match r+p
                                       ((regexp . proc)
                                        (match (list-matches regexp line)
                                          ((and m+ (_ _ ...))
                                           (proc line m+))
                                          (_ line)))))
                                   line*
                                   rx+proc))
                     (line1 (replace-char nul* #\nul line1*)))
                (display line1 out)
                (loop (read-line in 'concat)))))))))


(define-syntax let-matches
  ;; Helper macro for `substitute*'.
  (syntax-rules (_)
    ((let-matches index match (_ vars ...) body ...)
     (let-matches (+ 1 index) match (vars ...)
                  body ...))
    ((let-matches index match (var vars ...) body ...)
     (let ((var (match:substring match index)))
       (let-matches (+ 1 index) match (vars ...)
                    body ...)))
    ((let-matches index match () body ...)
     (begin body ...))))

(define-syntax substitute*
  (syntax-rules ()
    "Substitute REGEXP in FILE by the string returned by BODY.  BODY is
evaluated with each MATCH-VAR bound to the corresponding positional regexp
sub-expression.  For example:

  (substitute* file
     ((\"hello\")
      \"good morning\\n\")
     ((\"foo([a-z]+)bar(.*)$\" all letters end)
      (string-append \"baz\" letters end)))

Here, anytime a line of FILE contains \"hello\", it is replaced by \"good
morning\".  Anytime a line of FILE matches the second regexp, ALL is bound to
the complete match, LETTERS is bound to the first sub-expression, and END is
bound to the last one.

When one of the MATCH-VAR is `_', no variable is bound to the corresponding
match substring.

Alternatively, FILE may be a list of file names, in which case they are
all subject to the substitutions.

Be careful about using '$' to match the end of a line; by itself it won't
match the terminating newline of a line."
    ((substitute* file ((regexp match-var ...) body ...) ...)
     (let ()
       (define (substitute-one-file file-name)
         (substitute
          file-name
          (list (cons regexp
                      (lambda (l m+)
                        ;; Iterate over matches M+ and return the
                        ;; modified line based on L.
                        (let loop ((m* m+)  ; matches
                                   (o  0)   ; offset in L
                                   (r  '())) ; result
                          (match m*
                            (()
                             (let ((r (cons (substring l o) r)))
                               (string-concatenate-reverse r)))
                            ((m . rest)
                             (let-matches 0 m (match-var ...)
                               (loop rest
                                     (match:end m)
                                     (cons*
                                      (begin body ...)
                                      (substring l o (match:start m))
                                      r))))))))
                ...)))

       (match file
         ((files (... ...))
          (for-each substitute-one-file files))
         ((? string? f)
          (substitute-one-file f)))))))


;;;
;;; Patching shebangs---e.g., /bin/sh -> /gnu/store/xyz...-bash/bin/sh.
;;;

(define* (dump-port in out
                    #:optional len
                    #:key (buffer-size 16384)
                    (progress (lambda (t k) (k))))
  "Read LEN bytes from IN or as much data as possible if LEN is #f, and write
it to OUT, using chunks of BUFFER-SIZE bytes.  Call PROGRESS at the beginning
and after each successful transfer of BUFFER-SIZE bytes or less, passing it
the total number of bytes transferred and the continuation of the transfer as
a thunk."
  (define buffer
    (make-bytevector buffer-size))

  (define (loop total bytes)
    (or (eof-object? bytes)
        (and len (= total len))
        (let ((total (+ total bytes)))
          (put-bytevector out buffer 0 bytes)
          (progress total
                    (lambda ()
                      (loop total
                            (get-bytevector-n! in buffer 0
                                               (if len
                                                   (min (- len total) buffer-size)
                                                   buffer-size))))))))

  ;; Make sure PROGRESS is called when we start so that it can measure
  ;; throughput.
  (progress 0
            (lambda ()
              (loop 0 (get-bytevector-n! in buffer 0
                                         (if len
                                             (min len buffer-size)
                                             buffer-size))))))

(define AT_SYMLINK_NOFOLLOW
  ;; Guile 2.0 did not define this constant, hence this hack.
  (let ((variable (module-variable the-root-module 'AT_SYMLINK_NOFOLLOW)))
    (if variable
        (variable-ref variable)
        256)))                                    ;for GNU/Linux

(define (set-file-time file stat)
  "Set the atime/mtime of FILE to that specified by STAT."
  (utime file
         (stat:atime stat)
         (stat:mtime stat)
         (stat:atimensec stat)
         (stat:mtimensec stat)
         AT_SYMLINK_NOFOLLOW))

(define (get-char* p)
  ;; We call it `get-char', but that's really a binary version
  ;; thereof.  (The real `get-char' cannot be used here because our
  ;; bootstrap Guile is hacked to always use UTF-8.)
  (match (get-u8 p)
    ((? integer? x) (integer->char x))
    (x x)))

(define patch-shebang
  (let ((shebang-rx (make-regexp "^[[:blank:]]*(/[[:graph:]]+)[[:blank:]]*([[:graph:]]*)(.*)$")))
    (lambda* (file
              #:optional
              (path (search-path-as-string->list (getenv "PATH")))
              #:key (keep-mtime? #t))
      "Replace the #! interpreter file name in FILE by a valid one found in
PATH, when FILE actually starts with a shebang.  Return #t when FILE was
patched, #f otherwise.  When KEEP-MTIME? is true, the atime/mtime of
FILE are kept unchanged."
      (define (patch p interpreter rest-of-line)
        (let* ((template (string-append file ".XXXXXX"))
               (out      (mkstemp! template))
               (st       (stat file))
               (mode     (stat:mode st)))
          (with-throw-handler #t
            (lambda ()
              (format out "#!~a~a~%"
                      interpreter rest-of-line)
              (dump-port p out)
              (close out)
              (chmod template mode)
              (rename-file template file)
              (when keep-mtime?
                (set-file-time file st))
              #t)
            (lambda (key . args)
              (format (current-error-port)
                      "patch-shebang: ~a: error: ~a ~s~%"
                      file key args)
              (false-if-exception (delete-file template))
              #f))))

      (call-with-ascii-input-file file
        (lambda (p)
          (and (eq? #\# (get-char* p))
               (eq? #\! (get-char* p))
               (let ((line (false-if-exception (read-line p))))
                 (and=> (and line (regexp-exec shebang-rx line))
                        (lambda (m)
                          (let* ((interp (match:substring m 1))
                                 (arg1 (match:substring m 2))
                                 (rest (match:substring m 3))
                                 (has-env (string-suffix? "/env" interp))
                                 (cmd (if has-env arg1 (basename interp)))
                                 (bin (search-path path cmd)))
                            (if bin
                                (if (string=? bin interp)
                                    #f            ; nothing to do
                                    (if has-env
                                        (begin
                                          (format (current-error-port)
                                                  "patch-shebang: ~a: changing `~a' to `~a'~%"
                                                  file (string-append interp " " arg1) bin)
                                          (patch p bin rest))
                                      (begin 
                                        (format (current-error-port)
                                                "patch-shebang: ~a: changing `~a' to `~a'~%"
                                                file interp bin)
                                        (patch p bin
                                               (if (string-null? arg1)
                                                   ""
                                                   (string-append " " arg1 rest))))))
                                (begin
                                  (format (current-error-port)
                                          "patch-shebang: ~a: warning: no binary for interpreter `~a' found in $PATH~%"
                                          file (basename cmd))
                                  #f))))))))))))

(define* (patch-makefile-SHELL file #:key (keep-mtime? #t))
  "Patch the `SHELL' variable in FILE, which is supposedly a makefile.
When KEEP-MTIME? is true, the atime/mtime of FILE are kept unchanged."

  ;; For instance, Gettext-generated po/Makefile.in.in do not honor $SHELL.

  ;; XXX: Unlike with `patch-shebang', FILE is always touched.

  (define (find-shell name)
    (let ((shell (which name)))
      (unless shell
        (format (current-error-port)
                "patch-makefile-SHELL: warning: no binary for shell `~a' found in $PATH~%"
                name))
      shell))

  (let ((st (stat file)))
    ;; Consider FILE is using an 8-bit encoding to avoid errors.
    (with-fluids ((%default-port-encoding #f))
      (substitute* file
        (("^ *SHELL[[:blank:]]*:?=[[:blank:]]*([[:graph:]]*/)([[:graph:]]+)(.*)$"
          _ dir shell args)
         (let* ((old (string-append dir shell))
                (new (or (find-shell shell) old)))
           (unless (string=? new old)
             (format (current-error-port)
                     "patch-makefile-SHELL: ~a: changing `SHELL' from `~a' to `~a'~%"
                     file old new))
           (string-append "SHELL = " new args)))))

   (when keep-mtime?
     (set-file-time file st))))

(define* (patch-/usr/bin/file file
                              #:key
                              (file-command (which "file"))
                              (keep-mtime? #t))
  "Patch occurrences of \"/usr/bin/file\" in FILE, replacing them with
FILE-COMMAND.  When KEEP-MTIME? is true, keep FILE's modification time
unchanged."
  (if (not file-command)
      (format (current-error-port)
              "patch-/usr/bin/file: warning: \
no replacement 'file' command, doing nothing~%")
      (let ((st (stat file)))
        ;; Consider FILE is using an 8-bit encoding to avoid errors.
        (with-fluids ((%default-port-encoding #f))
          (substitute* file
            (("/usr/bin/file")
             (begin
               (format (current-error-port)
                       "patch-/usr/bin/file: ~a: changing `~a' to `~a'~%"
                       file "/usr/bin/file" file-command)
               file-command))))

        (when keep-mtime?
          (set-file-time file st)))))

(define* (fold-port-matches proc init pattern port
                            #:optional (unmatched (lambda (_ r) r)))
  "Read from PORT character-by-character; for each match against
PATTERN, call (PROC MATCH RESULT), where RESULT is seeded with INIT.
PATTERN is a list of SRFI-14 char-sets.  Call (UNMATCHED CHAR RESULT)
for each unmatched character."
  (define initial-pattern
    ;; The poor developer's regexp.
    (if (string? pattern)
        (map char-set (string->list pattern))
        pattern))

  ;; Note: we're not really striving for performance here...
  (let loop ((chars   '())
             (pattern initial-pattern)
             (matched '())
             (result  init))
    (cond ((null? chars)
           (loop (list (get-char* port))
                 pattern
                 matched
                 result))
          ((null? pattern)
           (loop chars
                 initial-pattern
                 '()
                 (proc (list->string (reverse matched)) result)))
          ((eof-object? (car chars))
           (fold-right unmatched result matched))
          ((char-set-contains? (car pattern) (car chars))
           (loop (cdr chars)
                 (cdr pattern)
                 (cons (car chars) matched)
                 result))
          ((null? matched)                        ; common case
           (loop (cdr chars)
                 pattern
                 matched
                 (unmatched (car chars) result)))
          (else
           (let ((matched (reverse matched)))
             (loop (append (cdr matched) chars)
                   initial-pattern
                   '()
                   (unmatched (car matched) result)))))))

(define* (remove-store-references file
                                  #:optional (store (%store-directory)))
  "Remove from FILE occurrences of file names in STORE; return #t when
store paths were encountered in FILE, #f otherwise.  This procedure is
known as `nuke-refs' in Nixpkgs."
  (define pattern
    (let ((nix-base32-chars
           '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
             #\a #\b #\c #\d #\f #\g #\h #\i #\j #\k #\l #\m #\n
             #\p #\q #\r #\s #\v #\w #\x #\y #\z)))
      `(,@(map char-set (string->list store))
        ,(char-set #\/)
        ,@(make-list 32 (list->char-set nix-base32-chars))
        ,(char-set #\-))))

  (with-fluids ((%default-port-encoding #f))
    (with-atomic-file-replacement file
      (lambda (in out)
        ;; We cannot use `regexp-exec' here because it cannot deal with
        ;; strings containing NUL characters.
        (format #t "removing store references from `~a'...~%" file)
        (setvbuf in 'block 65536)
        (setvbuf out 'block 65536)
        (fold-port-matches (lambda (match result)
                             (put-bytevector out (string->utf8 store))
                             (put-u8 out (char->integer #\/))
                             (put-bytevector out
                                             (string->utf8
                                              "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-"))
                             #t)
                           #f
                           pattern
                           in
                           (lambda (char result)
                             (put-u8 out (char->integer char))
                             result))))))

(define-condition-type &wrap-error &error
  wrap-error?
  (program    wrap-error-program)
  (type       wrap-error-type))

(define (wrapped-program? prog)
  "Return #t if PROG is a program that was moved and wrapped by 'wrap-program'."
  (and (file-exists? prog)
       (let ((base (basename prog)))
         (and (string-prefix? "." base)
              (string-suffix? "-real" base)))))

(define* (wrap-program prog #:key (sh (which "bash")) #:rest vars)
  "Make a wrapper for PROG.  VARS should look like this:

  '(VARIABLE DELIMITER POSITION LIST-OF-DIRECTORIES)

where DELIMITER is optional.  ':' will be used if DELIMITER is not given.

For example, this command:

  (wrap-program \"foo\"
                '(\"PATH\" \":\" = (\"/gnu/.../bar/bin\"))
                '(\"CERT_PATH\" suffix (\"/gnu/.../baz/certs\"
                                        \"/qux/certs\")))

will copy 'foo' to '.foo-real' and create the file 'foo' with the following
contents:

  #!location/of/bin/bash
  export PATH=\"/gnu/.../bar/bin\"
  export CERT_PATH=\"$CERT_PATH${CERT_PATH:+:}/gnu/.../baz/certs:/qux/certs\"
  exec -a $0 location/of/.foo-real \"$@\"

This is useful for scripts that expect particular programs to be in $PATH, for
programs that expect particular shared libraries to be in $LD_LIBRARY_PATH, or
modules in $GUILE_LOAD_PATH, etc.

If PROG has previously been wrapped by 'wrap-program', the wrapper is extended
with definitions for VARS. If it is not, SH will be used as interpreter."
  (define vars/filtered
    (match vars
      ((#:sh _ . vars) vars)
      (vars vars)))

  (define wrapped-file
    (string-append (dirname prog) "/." (basename prog) "-real"))

  (define already-wrapped?
    (file-exists? wrapped-file))

  (define (last-line port)
    ;; Return the last line read from PORT and leave PORT's cursor right
    ;; before it.
    (let loop ((previous-line-offset 0)
               (previous-line "")
               (position (seek port 0 SEEK_CUR)))
      (match (read-line port 'concat)
        ((? eof-object?)
         (seek port previous-line-offset SEEK_SET)
         previous-line)
        ((? string? line)
         (loop position line (+ (string-length line) position))))))

  (define (export-variable lst)
    ;; Return a string that exports an environment variable.
    (match lst
      ((var sep '= rest)
       (format #f "export ~a=\"~a\""
               var (string-join rest sep)))
      ((var sep 'prefix rest)
       (format #f "export ~a=\"~a${~a:+~a}$~a\""
               var (string-join rest sep) var sep var))
      ((var sep 'suffix rest)
       (format #f "export ~a=\"$~a${~a+~a}~a\""
               var var var sep (string-join rest sep)))
      ((var '= rest)
       (format #f "export ~a=\"~a\""
               var (string-join rest ":")))
      ((var 'prefix rest)
       (format #f "export ~a=\"~a${~a:+:}$~a\""
               var (string-join rest ":") var var))
      ((var 'suffix rest)
       (format #f "export ~a=\"$~a${~a:+:}~a\""
               var var var (string-join rest ":")))))

  (when (wrapped-program? prog)
    (error (string-append prog " is a wrapper. Refusing to wrap.")))

  (if already-wrapped?

      ;; PROG is already a wrapper: add the new "export VAR=VALUE" lines just
      ;; before the last line.
      (let* ((port (open-file prog "r+"))
             (last (last-line port)))
        (for-each (lambda (var)
                    (display (export-variable var) port)
                    (newline port))
                  vars/filtered)
        (display last port)
        (close-port port))

      ;; PROG is not wrapped yet: create a shell script that sets VARS.
      (let ((prog-tmp (string-append wrapped-file "-tmp")))
        (link prog wrapped-file)

        (call-with-output-file prog-tmp
          (lambda (port)
            (format port
                    "#!~a~%~a~%exec -a \"$0\" \"~a\" \"$@\"~%"
                    sh
                    (string-join (map export-variable vars/filtered) "\n")
                    (canonicalize-path wrapped-file))))

        (chmod prog-tmp #o755)
        (rename-file prog-tmp prog))))

(define wrap-script
  (let ((interpreter-regex
         (make-regexp
          (string-append "^#! ?(/[^ ]+/bin/("
                         (string-join '("python[^ ]*"
                                        "Rscript"
                                        "perl"
                                        "ruby"
                                        "bash"
                                        "sh") "|")
                         "))( ?.*)")))
        (coding-line-regex
         (make-regexp
          ".*#.*coding[=:][[:space:]]*([-a-zA-Z_0-9.]+)")))
    (lambda* (prog #:key (guile (which "guile")) #:rest vars)
      "Wrap the script PROG such that VARS are set first.  The format of VARS
is the same as in the WRAP-PROGRAM procedure.  This procedure differs from
WRAP-PROGRAM in that it does not create a separate shell script.  Instead,
PROG is modified directly by prepending a Guile script, which is interpreted
as a comment in the script's language.

Special encoding comments as supported by Python are recreated on the second
line.

Note that this procedure can only be used once per file as Guile scripts are
not supported."
      (define update-env
        (match-lambda
          ((var sep '= rest)
           `(setenv ,var ,(string-join rest sep)))
          ((var sep 'prefix rest)
           `(let ((current (getenv ,var)))
              (setenv ,var (if current
                               (string-append ,(string-join rest sep)
                                              ,sep current)
                               ,(string-join rest sep)))))
          ((var sep 'suffix rest)
           `(let ((current (getenv ,var)))
              (setenv ,var (if current
                               (string-append current ,sep
                                              ,(string-join rest sep))
                               ,(string-join rest sep)))))
          ((var '= rest)
           `(setenv ,var ,(string-join rest ":")))
          ((var 'prefix rest)
           `(let ((current (getenv ,var)))
              (setenv ,var (if current
                               (string-append ,(string-join rest ":")
                                              ":" current)
                               ,(string-join rest ":")))))
          ((var 'suffix rest)
           `(let ((current (getenv ,var)))
              (setenv ,var (if current
                               (string-append current ":"
                                              ,(string-join rest ":"))
                               ,(string-join rest ":")))))))
      (let-values (((interpreter args coding-line)
                    (call-with-ascii-input-file prog
                      (lambda (p)
                        (let ((first-match
                               (false-if-exception
                                (regexp-exec interpreter-regex (read-line p)))))
                          (values (and first-match (match:substring first-match 1))
                                  (and first-match (match:substring first-match 3))
                                  (false-if-exception
                                   (and=> (regexp-exec coding-line-regex (read-line p))
                                          (lambda (m) (match:substring m 0))))))))))
        (if interpreter
            (let* ((header (format #f "\
#!~a --no-auto-compile
#!#; ~a
#\\-~s
#\\-~s
"
                                   guile
                                   (or coding-line "Guix wrapper")
                                   (cons 'begin (map update-env
                                                     (match vars
                                                       ((#:guile _ . vars) vars)
                                                       (_ vars))))
                                   `(let ((cl (command-line)))
                                      (apply execl ,interpreter
                                             (car cl)
                                             (append
                                              ',(string-tokenize args char-set:graphic)
                                              cl)))))
                   (template (string-append prog ".XXXXXX"))
                   (out      (mkstemp! template))
                   (st       (stat prog))
                   (mode     (stat:mode st)))
              (with-throw-handler #t
                (lambda ()
                  (call-with-ascii-input-file prog
                    (lambda (p)
                      (display header out)
                      (dump-port p out)
                      (close out)
                      (chmod template mode)
                      (rename-file template prog)
                      (set-file-time prog st))))
                (lambda (key . args)
                  (format (current-error-port)
                          "wrap-script: ~a: error: ~a ~s~%"
                          prog key args)
                  (false-if-exception (delete-file template))
                  (raise (condition
                          (&wrap-error (program prog)
                                       (type key))))
                  #f)))
            (raise (condition
                    (&wrap-error (program prog)
                                 (type 'no-interpreter-found)))))))))

(define* (make-desktop-entry-file destination #:key
                                  (type "Application") ; One of "Application", "Link" or "Directory".
                                  (version "1.1")
                                  name
                                  (generic-name name)
                                  (no-display #f)
                                  comment
                                  icon
                                  (hidden #f)
                                  only-show-in
                                  not-show-in
                                  (d-bus-activatable #f)
                                  try-exec
                                  exec
                                  path
                                  (terminal #f)
                                  actions
                                  mime-type
                                  (categories "Application")
                                  implements
                                  keywords
                                  (startup-notify #t)
                                  startup-w-m-class
                                  #:rest all-args)
  "Create a desktop entry file at DESTINATION.
You must specify NAME.

Values can be booleans, numbers, strings or list of strings.

Additionally, locales can be specified with an alist where the key is the
locale.  The #f key specifies the default.  Example:

  #:name '((#f \"I love Guix\") (\"fr\" \"J'aime Guix\"))

produces

  Name=I love Guix
  Name[fr]=J'aime Guix

For a complete description of the format, see the specifications at
https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-latest.html."
  (define (escape-semicolon s)
    (string-join (string-split s #\;) "\\;"))
  (define* (parse key value #:optional locale)
    (set! value (match value
                  (#t "true")
                  (#f "false")
                  ((?  number? n) n)
                  ((?  string? s) (escape-semicolon s))
                  ((?  list? value)
                   (catch 'wrong-type-arg
                     (lambda () (string-join (map escape-semicolon value) ";"))
                     (lambda args (error "List arguments can only contain strings: ~a" args))))
                  (_ (error "Value must be a boolean, number, string or list of strings"))))
    (format #t "~a=~a~%"
            (if locale
                (format #f "~a[~a]" key locale)
                key)
            value))

  (define key-error-message "This procedure only takes key arguments beside DESTINATION")

  (unless name
    (error "Missing NAME key argument"))
  (unless (member #:type all-args)
    (set! all-args (append (list #:type type) all-args)))
  (mkdir-p (dirname destination))

  (with-output-to-file destination
    (lambda ()
      (format #t "[Desktop Entry]~%")
      (let loop ((args all-args))
        (match args
          (() #t)
          ((_) (error key-error-message))
          ((key value . ...)
           (unless (keyword? key)
             (error key-error-message))
           (set! key
                 (string-join (map string-titlecase
                                   (string-split (symbol->string
                                                  (keyword->symbol key))
                                                 #\-))
                              ""))
           (match value
             (((_ . _) . _)
              (for-each (lambda (locale-subvalue)
                          (parse key
                                 (if (and (list? (cdr locale-subvalue))
                                          (= 1 (length (cdr locale-subvalue))))
                                     ;; Support both proper and improper lists for convenience.
                                     (cadr locale-subvalue)
                                     (cdr locale-subvalue))
                                 (car locale-subvalue)))
                        value))
             (_
              (parse key value)))
           (loop (cddr args))))))))


;;;
;;; Locales.
;;;

(define (locale-category->string category)
  "Return the name of locale category CATEGORY, one of the 'LC_' constants.
If CATEGORY is a bitwise or of several 'LC_' constants, an approximation is
returned."
  (letrec-syntax ((convert (syntax-rules ()
                             ((_)
                              (number->string category))
                             ((_ first rest ...)
                              (if (= first category)
                                  (symbol->string 'first)
                                  (convert rest ...))))))
    (convert LC_ADDRESS LC_ALL LC_COLLATE LC_CTYPE
             LC_IDENTIFICATION LC_MEASUREMENT LC_MESSAGES LC_MONETARY
             LC_NAME LC_NUMERIC LC_PAPER LC_TELEPHONE
             LC_TIME)))

;;; Local Variables:
;;; eval: (put 'call-with-output-file/atomic 'scheme-indent-function 1)
;;; eval: (put 'call-with-ascii-input-file 'scheme-indent-function 1)
;;; eval: (put 'with-throw-handler 'scheme-indent-function 1)
;;; eval: (put 'let-matches 'scheme-indent-function 3)
;;; eval: (put 'with-atomic-file-replacement 'scheme-indent-function 1)
;;; End:
