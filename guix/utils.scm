;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018, 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Chris Marusich <cmmarusich@gmail.com>
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

(define-module (guix utils)
  #:use-module (guix config)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-39)
  #:use-module (ice-9 ftw)
  #:use-module (rnrs io ports)                    ;need 'port-position' etc.
  #:use-module ((rnrs bytevectors) #:select (bytevector-u8-set!))
  #:use-module (guix memoization)
  #:use-module ((guix build utils) #:select (dump-port mkdir-p delete-file-recursively))
  #:use-module ((guix build syscalls) #:select (mkdtemp! fdatasync))
  #:use-module ((guix combinators) #:select (fold2))
  #:use-module (guix diagnostics)           ;<location>, &error-location, etc.
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module ((ice-9 iconv) #:prefix iconv:)
  #:autoload   (zlib) (make-zlib-input-port make-zlib-output-port)
  #:use-module (system foreign)
  #:re-export (<location>                         ;for backwards compatibility
               location
               location?
               location-file
               location-line
               location-column
               source-properties->location
               location->source-properties

               &error-location
               error-location?
               error-location

               &fix-hint
               fix-hint?
               condition-fix-hint)
  #:export (strip-keyword-arguments
            default-keyword-arguments
            substitute-keyword-arguments
            ensure-keyword-arguments

            current-source-directory

            nix-system->gnu-triplet
            gnu-triplet->nix-system
            %current-system
            %current-target-system
            package-name->name+version
            target-mingw?
            target-arm32?
            target-aarch64?
            target-arm?
            target-powerpc?
            target-64bit?
            cc-for-target
            cxx-for-target
            pkg-config-for-target

            version-compare
            version>?
            version>=?
            version-prefix
            version-major+minor+point
            version-major+minor
            version-major
            version-unique-prefix
            guile-version>?
            version-prefix?
            string-replace-substring
            file-extension
            file-sans-extension
            tarball-sans-extension
            compressed-file?
            switch-symlinks
            call-with-temporary-output-file
            call-with-temporary-directory
            with-atomic-file-output

            with-environment-variables
            arguments-from-environment-variable

            config-directory
            cache-directory

            readlink*
            edit-expression

            filtered-port
            decompressed-port
            call-with-decompressed-port
            compressed-output-port
            call-with-compressed-output-port
            canonical-newline-port

            string-distance
            string-closest))


;;;
;;; Environment variables.
;;;

(define (call-with-environment-variables variables thunk)
  "Call THUNK with the environment VARIABLES set."
  (let ((environment (environ)))
    (dynamic-wind
      (lambda ()
        (for-each (match-lambda
                    ((variable value)
                     (setenv variable value)))
                  variables))
      thunk
      (lambda ()
        (environ environment)))))

(define-syntax-rule (with-environment-variables variables exp ...)
  "Evaluate EXP with the given environment VARIABLES set."
  (call-with-environment-variables variables
                                   (lambda () exp ...)))

(define (arguments-from-environment-variable variable)
  "Retrieve value of environment variable denoted by string VARIABLE in the
form of a list of strings (`char-set:graphic' tokens) suitable for consumption
by `args-fold', if VARIABLE is defined, otherwise return an empty list."
  (let ((env (getenv variable)))
    (if env
        (string-tokenize env char-set:graphic)
        '())))


;;;
;;; Filtering & pipes.
;;;

(define (filtered-port command input)
  "Return an input port where data drained from INPUT is filtered through
COMMAND (a list).  In addition, return a list of PIDs that the caller must
wait.  When INPUT is a file port, it must be unbuffered; otherwise, any
buffered data is lost."
  (let loop ((input input)
             (pids  '()))
    (if (file-port? input)
        (match (pipe)
          ((in . out)
           (match (primitive-fork)
             (0
              (dynamic-wind
                (const #f)
                (lambda ()
                  (close-port in)
                  (close-port (current-input-port))
                  (dup2 (fileno input) 0)
                  (close-port (current-output-port))
                  (dup2 (fileno out) 1)
                  (catch 'system-error
                    (lambda ()
                      (apply execl (car command) command))
                    (lambda args
                      (format (current-error-port)
                              "filtered-port: failed to execute '~{~a ~}': ~a~%"
                              command (strerror (system-error-errno args))))))
                (lambda ()
                  (primitive-_exit 1))))
             (child
              (close-port out)
              (values in (cons child pids))))))

        ;; INPUT is not a file port, so fork just for the sake of tunneling it
        ;; through a file port.
        (match (pipe)
          ((in . out)
           (match (primitive-fork)
             (0
              (dynamic-wind
                (const #t)
                (lambda ()
                  (close-port in)
                  (dump-port input out))
                (lambda ()
                  (close-port input)
                  (false-if-exception (close out))
                  (primitive-_exit 0))))
             (child
              (close-port input)
              (close-port out)
              (loop in (cons child pids)))))))))

(define (lzip-port proc port . args)
  "Return the lzip port produced by calling PROC (a symbol) on PORT and ARGS.
Raise an error if lzlib support is missing."
  (let ((make-port (module-ref (resolve-interface '(lzlib)) proc)))
    (make-port port)))

(define (zstd-port proc port . args)
  "Return the zstd port produced by calling PROC (a symbol) on PORT and ARGS.
Raise an error if zstd support is missing."
  (let ((make-port (module-ref (resolve-interface '(zstd)) proc)))
    (make-port port)))

(define (decompressed-port compression input)
  "Return an input port where INPUT is decompressed according to COMPRESSION,
a symbol such as 'xz."
  (match compression
    ((or #f 'none) (values input '()))
    ('bzip2        (filtered-port `(,%bzip2 "-dc") input))
    ('xz           (filtered-port `(,%xz "-dc") input))
    ('gzip         (values (make-zlib-input-port input #:format 'gzip)
                           '()))
    ('lzip         (values (lzip-port 'make-lzip-input-port input)
                           '()))
    ('zstd         (values (zstd-port 'make-zstd-input-port input)
                           '()))
    (_             (error "unsupported compression scheme" compression))))

(define (call-with-decompressed-port compression port proc)
  "Call PROC with a wrapper around PORT, a file port, that decompresses data
read from PORT according to COMPRESSION, a symbol such as 'xz."
  (let-values (((decompressed pids)
                (decompressed-port compression port)))
    (dynamic-wind
      (const #f)
      (lambda ()
        (proc decompressed))
      (lambda ()
        (close-port decompressed)
        (unless (every (compose zero? cdr waitpid) pids)
          (error "decompressed-port failure" pids))))))

(define (filtered-output-port command output)
  "Return an output port.  Data written to that port is filtered through
COMMAND and written to OUTPUT, an output file port.  In addition, return a
list of PIDs to wait for.  OUTPUT must be unbuffered; otherwise, any buffered
data is lost."
  (match (pipe)
    ((in . out)
     (match (primitive-fork)
       (0
        (dynamic-wind
          (const #f)
          (lambda ()
            (close-port out)
            (close-port (current-input-port))
            (dup2 (fileno in) 0)
            (close-port (current-output-port))
            (dup2 (fileno output) 1)
            (catch 'system-error
              (lambda ()
                (apply execl (car command) command))
              (lambda args
                (format (current-error-port)
                        "filtered-output-port: failed to execute '~{~a ~}': ~a~%"
                        command (strerror (system-error-errno args))))))
          (lambda ()
            (primitive-_exit 1))))
       (child
        (close-port in)
        (values out (list child)))))))

(define* (compressed-output-port compression output
                                 #:key (options '()))
  "Return an output port whose input is compressed according to COMPRESSION,
a symbol such as 'xz, and then written to OUTPUT.  In addition return a list
of PIDs to wait for.  OPTIONS is a list of strings passed to the compression
program--e.g., '(\"--fast\")."
  (match compression
    ((or #f 'none) (values output '()))
    ('bzip2        (filtered-output-port `(,%bzip2 "-c" ,@options) output))
    ('xz           (filtered-output-port `(,%xz "-c" ,@options) output))
    ('gzip         (values (make-zlib-output-port output #:format 'gzip)
                           '()))
    ('lzip         (values (lzip-port 'make-lzip-output-port output)
                           '()))
    ('zstd         (values (zstd-port 'make-zstd-output-port output)
                           '()))
    (_             (error "unsupported compression scheme" compression))))

(define* (call-with-compressed-output-port compression port proc
                                           #:key (options '()))
  "Call PROC with a wrapper around PORT, a file port, that compresses data
that goes to PORT according to COMPRESSION, a symbol such as 'xz.  OPTIONS is
a list of command-line arguments passed to the compression program."
  (let-values (((compressed pids)
                (compressed-output-port compression port
                                        #:options options)))
    (dynamic-wind
      (const #f)
      (lambda ()
        (proc compressed))
      (lambda ()
        (close-port compressed)
        (unless (every (compose zero? cdr waitpid) pids)
          (error "compressed-output-port failure" pids))))))

(define* (edit-expression source-properties proc #:key (encoding "UTF-8"))
  "Edit the expression specified by SOURCE-PROPERTIES using PROC, which should
be a procedure that takes the original expression in string and returns a new
one.  ENCODING will be used to interpret all port I/O, it default to UTF-8.
This procedure returns #t on success."
  (with-fluids ((%default-port-encoding encoding))
    (let* ((file   (assq-ref source-properties 'filename))
           (line   (assq-ref source-properties 'line))
           (column (assq-ref source-properties 'column))
           (in     (open-input-file file))
           ;; The start byte position of the expression.
           (start  (begin (while (not (and (= line (port-line in))
                                           (= column (port-column in))))
                            (when (eof-object? (read-char in))
                              (error (format #f "~a: end of file~%" in))))
                          (ftell in)))
           ;; The end byte position of the expression.
           (end    (begin (read in) (ftell in))))
      (seek in 0 SEEK_SET) ; read from the beginning of the file.
      (let* ((pre-bv  (get-bytevector-n in start))
             ;; The expression in string form.
             (str     (iconv:bytevector->string
                       (get-bytevector-n in (- end start))
                       (port-encoding in)))
             (post-bv (get-bytevector-all in))
             (str*    (proc str)))
        ;; Verify the edited expression is still a scheme expression.
        (call-with-input-string str* read)
        ;; Update the file with edited expression.
        (with-atomic-file-output file
          (lambda (out)
            (put-bytevector out pre-bv)
            (display str* out)
            ;; post-bv maybe the end-of-file object.
            (when (not (eof-object? post-bv))
              (put-bytevector out post-bv))
            #t))))))


;;;
;;; Keyword arguments.
;;;

(define (strip-keyword-arguments keywords args)
  "Remove all of the keyword arguments listed in KEYWORDS from ARGS."
  (let loop ((args   args)
             (result '()))
    (match args
      (()
       (reverse result))
      (((? keyword? kw) arg . rest)
       (loop rest
             (if (memq kw keywords)
                 result
                 (cons* arg kw result))))
      ((head . tail)
       (loop tail (cons head result))))))

(define (default-keyword-arguments args defaults)
  "Return ARGS augmented with any keyword/value from DEFAULTS for
keywords not already present in ARGS."
  (let loop ((defaults defaults)
             (args     args))
    (match defaults
      ((kw value rest ...)
       (loop rest
             (if (memq kw args)
                 args
                 (cons* kw value args))))
      (()
       args))))

(define-syntax collect-default-args
  (syntax-rules ()
    ((_)
     '())
    ((_ (_ _) rest ...)
     (collect-default-args rest ...))
    ((_ (kw _ dflt) rest ...)
     (cons* kw dflt (collect-default-args rest ...)))))

(define-syntax substitute-keyword-arguments
  (syntax-rules ()
    "Return a new list of arguments where the value for keyword arg KW is
replaced by EXP.  EXP is evaluated in a context where VAR is bound to the
previous value of the keyword argument, or DFLT if given."
    ((_ original-args ((kw var dflt ...) exp) ...)
     (let loop ((args (default-keyword-arguments
                        original-args
                        (collect-default-args (kw var dflt ...) ...)))
                (before '()))
       (match args
         ((kw var rest (... ...))
          (loop rest (cons* exp kw before)))
         ...
         ((x rest (... ...))
          (loop rest (cons x before)))
         (()
          (reverse before)))))))

(define (delkw kw lst)
  "Remove KW and its associated value from LST, a keyword/value list such
as '(#:foo 1 #:bar 2)."
  (let loop ((lst    lst)
             (result '()))
    (match lst
      (()
       (reverse result))
      ((kw? value rest ...)
       (if (eq? kw? kw)
           (append (reverse result) rest)
           (loop rest (cons* value kw? result)))))))

(define (ensure-keyword-arguments args kw/values)
  "Force the keywords arguments KW/VALUES in the keyword argument list ARGS.
For instance:

  (ensure-keyword-arguments '(#:foo 2) '(#:foo 2))
  => (#:foo 2)

  (ensure-keyword-arguments '(#:foo 2) '(#:bar 3))
  => (#:foo 2 #:bar 3)

  (ensure-keyword-arguments '(#:foo 2) '(#:bar 3 #:foo 42))
  => (#:foo 42 #:bar 3)
"
  (let loop ((args      args)
             (kw/values kw/values)
             (result    '()))
    (match args
      (()
       (append (reverse result) kw/values))
      ((kw value rest ...)
       (match (memq kw kw/values)
         ((_ value . _)
          (loop rest (delkw kw kw/values) (cons* value kw result)))
         (#f
          (loop rest kw/values (cons* value kw result))))))))


;;;
;;; System strings.
;;;

(define* (nix-system->gnu-triplet
          #:optional (system (%current-system)) (vendor "unknown"))
  "Return a guess of the GNU triplet corresponding to Nix system
identifier SYSTEM."
  (match system
    ("armhf-linux"
     (string-append "arm-" vendor "-linux-gnueabihf"))
    (_
     (let* ((dash (string-index system #\-))
            (arch (substring system 0 dash))
            (os   (substring system (+ 1 dash))))
       (string-append arch
                      "-" vendor "-"
                      (if (string=? os "linux")
                          "linux-gnu"
                          os))))))

(define (gnu-triplet->nix-system triplet)
  "Return the Nix system type corresponding to TRIPLET, a GNU triplet as
returned by `config.guess'."
  (let ((triplet (cond ((string-match "^i[345]86-(.*)$" triplet)
                        =>
                        (lambda (m)
                          (string-append "i686-" (match:substring m 1))))
                       (else triplet))))
    (cond ((string-match "^arm[^-]*-([^-]+-)?linux-gnueabihf" triplet)
           "armhf-linux")
          ((string-match "^([^-]+)-([^-]+-)?linux-gnu.*" triplet)
           =>
           (lambda (m)
             ;; Nix omits `-gnu' for GNU/Linux.
             (string-append (match:substring m 1) "-linux")))
          ((string-match "^([^-]+)-([^-]+-)?([[:alpha:]]+)([0-9]+\\.?)*$" triplet)
           =>
           (lambda (m)
             ;; Nix strip the version number from names such as `gnu0.3',
             ;; `darwin10.2.0', etc., and always strips the vendor part.
             (string-append (match:substring m 1) "-"
                            (match:substring m 3))))
          (else triplet))))

(define %current-system
  ;; System type as expected by Nix, usually ARCHITECTURE-KERNEL.
  ;; By default, this is equal to (gnu-triplet->nix-system %host-type).
  (make-parameter %system))

(define %current-target-system
  ;; Either #f or a GNU triplet representing the target system we are
  ;; cross-building to.
  (make-parameter #f))

(define* (package-name->name+version spec
                                     #:optional (delimiter #\@))
  "Given SPEC, a package name like \"foo@0.9.1b\", return two values: \"foo\"
and \"0.9.1b\".  When the version part is unavailable, SPEC and #f are
returned.  Both parts must not contain any '@'.  Optionally, DELIMITER can be
a character other than '@'."
  (match (string-rindex spec delimiter)
    (#f  (values spec #f))
    (idx (values (substring spec 0 idx)
                 (substring spec (1+ idx))))))

(define* (target-mingw? #:optional (target (%current-target-system)))
  (and target
       (string-suffix? "-mingw32" target)))

(define* (target-arm32? #:optional (target (or (%current-target-system)
                                               (%current-system))))
  (string-prefix? "arm" target))

(define* (target-aarch64? #:optional (target (or (%current-target-system)
                                                 (%current-system))))
  (string-prefix? "aarch64" target))

(define* (target-arm? #:optional (target (or (%current-target-system)
                                             (%current-system))))
  (or (target-arm32? target) (target-aarch64? target)))

(define* (target-powerpc? #:optional (target (or (%current-target-system)
                                                 (%current-system))))
  (string-prefix? "powerpc" target))

(define* (target-64bit? #:optional (system (or (%current-target-system)
                                               (%current-system))))
  (any (cut string-prefix? <> system) '("x86_64" "aarch64" "mips64" "powerpc64")))

(define* (cc-for-target #:optional (target (%current-target-system)))
  (if target
      (string-append target "-gcc")
      "gcc"))

(define* (cxx-for-target #:optional (target (%current-target-system)))
  (if target
      (string-append target "-g++")
      "g++"))

(define* (pkg-config-for-target #:optional (target (%current-target-system)))
  (if target
      (string-append target "-pkg-config")
      "pkg-config"))

(define version-compare
  (let ((strverscmp
         (let ((sym (or (dynamic-func "strverscmp" (dynamic-link))
                        (error "could not find `strverscmp' (from GNU libc)"))))
           (pointer->procedure int sym (list '* '*)))))
    (lambda (a b)
      "Return '> when A denotes a newer version than B,
'< when A denotes a older version than B,
or '= when they denote equal versions."
      (let ((result (strverscmp (string->pointer a) (string->pointer b))))
        (cond ((positive? result) '>)
              ((negative? result) '<)
              (else '=))))))

(define (version-prefix version-string num-parts)
  "Truncate version-string to the first num-parts components of the version.
For example, (version-prefix \"2.1.47.4.23\" 3) returns \"2.1.47\""
  (string-join (take (string-split version-string #\.) num-parts) "."))

(define (version-major+minor+point version-string)
  "Return \"major>.<minor>.<point>\", where major, minor and point are the
major, minor and point version numbers from the version-string.  For example,
(version-major+minor+point \"6.4.5.2\") returns \"6.4.5\" or
(version-major+minor+point \"1.19.2-2581-324ca14c3003\") returns \"1.19.2\"."
  (let* ((3-dot (version-prefix version-string 3))
         (index (string-index 3-dot #\-)))
    (or (false-if-exception (substring 3-dot 0 index))
        3-dot)))

(define (version-major+minor version-string)
  "Return \"<major>.<minor>\", where major and minor are the major and
minor version numbers from version-string."
  (version-prefix version-string 2))

(define (version-major version-string)
  "Return the major version number as string from the version-string."
  (version-prefix version-string 1))

(define (version-unique-prefix version versions)
  "Return the shortest version prefix to unambiguously identify VERSION among
VERSIONS.  For example:

  (version-unique-prefix \"2.0\" '(\"3.0\" \"2.0\"))
  => \"2\"

  (version-unique-prefix \"2.2\" '(\"3.0.5\" \"2.0.9\" \"2.2.7\"))
  => \"2.2\"

  (version-unique-prefix \"27.1\" '(\"27.1\"))
  => \"\"
"
  (define not-dot
    (char-set-complement (char-set #\.)))

  (define other-versions
    (delete version versions))

  (let loop ((prefix     '())
             (components (string-tokenize version not-dot)))
    (define prefix-str
      (string-join prefix "."))

    (if (any (cut string-prefix? prefix-str <>) other-versions)
        (match components
          ((head . tail)
           (loop `(,@prefix ,head) tail))
          (()
           version))
        prefix-str)))

(define (version>? a b)
  "Return #t when A denotes a version strictly newer than B."
  (eq? '> (version-compare a b)))

(define (version>=? a b)
  "Return #t when A denotes a version newer or equal to B."
  (case (version-compare a b)
    ((> =) #t)
    (else #f)))

(define (guile-version>? str)
  "Return #t if the running Guile version is greater than STR."
  ;; Note: Using (version>? (version) "2.0.5") or similar doesn't work,
  ;; because the result of (version) can have a prefix, like "2.0.5-deb1".
  (version>? (string-append (major-version) "."
                            (minor-version) "."
                            (micro-version))
             str))

(define version-prefix?
  (let ((not-dot (char-set-complement (char-set #\.))))
    (lambda (v1 v2)
      "Return true if V1 is a version prefix of V2:

  (version-prefix? \"4.1\" \"4.16.2\") => #f
  (version-prefix? \"4.1\" \"4.1.2\") => #t
"
      (define (list-prefix? lst1 lst2)
        (match lst1
          (() #t)
          ((head1 tail1 ...)
           (match lst2
             (() #f)
             ((head2 tail2 ...)
              (and (equal? head1 head2)
                   (list-prefix? tail1 tail2)))))))

      (list-prefix? (string-tokenize v1 not-dot)
                    (string-tokenize v2 not-dot)))))


;;;
;;; Files.
;;;

(define (file-extension file)
  "Return the extension of FILE or #f if there is none."
  (let ((dot (string-rindex file #\.)))
    (and dot (substring file (+ 1 dot) (string-length file)))))

(define (file-sans-extension file)
  "Return the substring of FILE without its extension, if any."
  (let ((dot (string-rindex file #\.)))
    (if dot
        (substring file 0 dot)
        file)))

(define (tarball-sans-extension tarball)
  "Return TARBALL without its .tar.* or .zip extension."
  (let ((end (or (string-contains tarball ".tar")
                 (string-contains tarball ".tgz")
                 (string-contains tarball ".zip"))))
    (substring tarball 0 end)))

(define (compressed-file? file)
  "Return true if FILE denotes a compressed file."
  (->bool (member (file-extension file)
                  '("gz" "bz2" "xz" "lz" "lzma" "tgz" "tbz2" "zip"))))

(define (switch-symlinks link target)
  "Atomically switch LINK, a symbolic link, to point to TARGET.  Works
both when LINK already exists and when it does not."
  (let ((pivot (string-append link ".new")))
    (symlink target pivot)
    (rename-file pivot link)))

(define* (string-replace-substring str substr replacement
                                   #:optional
                                   (start 0)
                                   (end (string-length str)))
  "Replace all occurrences of SUBSTR in the START--END range of STR by
REPLACEMENT."
  (match (string-length substr)
    (0
     (error "string-replace-substring: empty substring"))
    (substr-length
     (let loop ((start  start)
                (pieces (list (substring str 0 start))))
       (match (string-contains str substr start end)
         (#f
          (string-concatenate-reverse
           (cons (substring str start) pieces)))
         (index
          (loop (+ index substr-length)
                (cons* replacement
                       (substring str start index)
                       pieces))))))))

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

(define (call-with-temporary-directory proc)
  "Call PROC with a name of a temporary directory; close the directory and
delete it when leaving the dynamic extent of this call."
  (let* ((directory (or (getenv "TMPDIR") "/tmp"))
         (template  (string-append directory "/guix-directory.XXXXXX"))
         (tmp-dir   (mkdtemp! template)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc tmp-dir))
      (lambda ()
        (false-if-exception (delete-file-recursively tmp-dir))))))

(define (with-atomic-file-output file proc)
  "Call PROC with an output port for the file that is going to replace FILE.
Upon success, FILE is atomically replaced by what has been written to the
output port, and PROC's result is returned."
  (let* ((template (string-append file ".XXXXXX"))
         (out      (mkstemp! template)))
    (with-throw-handler #t
      (lambda ()
        (let ((result (proc out)))
          (fdatasync out)
          (close-port out)
          (rename-file template file)
          result))
      (lambda (key . args)
        (false-if-exception (delete-file template))
        (close-port out)))))

(define* (xdg-directory variable suffix #:key (ensure? #t))
  "Return the name of the XDG directory that matches VARIABLE and SUFFIX,
after making sure that it exists if ENSURE? is true.  VARIABLE is an
environment variable name like \"XDG_CONFIG_HOME\"; SUFFIX is a suffix like
\"/.config\".  Honor the XDG specs,
<http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html>."
  (let ((dir (and=> (or (getenv variable)
                        (and=> (or (getenv "HOME")
                                   (passwd:dir (getpwuid (getuid))))
                               (cut string-append <> suffix)))
                    (cut string-append <> "/guix"))))
    (when ensure?
      (mkdir-p dir))
    dir))

(define config-directory
  (cut xdg-directory "XDG_CONFIG_HOME" "/.config" <...>))

(define cache-directory
  (cut xdg-directory "XDG_CACHE_HOME" "/.cache" <...>))

(define (readlink* file)
  "Call 'readlink' until the result is not a symlink."
  (define %max-symlink-depth 50)

  (let loop ((file  file)
             (depth 0))
    (define (absolute target)
      (if (absolute-file-name? target)
          target
          (string-append (dirname file) "/" target)))

    (if (>= depth %max-symlink-depth)
        file
        (call-with-values
            (lambda ()
              (catch 'system-error
                (lambda ()
                  (values #t (readlink file)))
                (lambda args
                  (let ((errno (system-error-errno args)))
                    (if (or (= errno EINVAL))
                        (values #f file)
                        (apply throw args))))))
          (lambda (success? target)
            (if success?
                (loop (absolute target) (+ depth 1))
                file))))))

(define (canonical-newline-port port)
  "Return an input port that wraps PORT such that all newlines consist
  of a single linefeed."
  (define (get-position)
    (if (port-has-port-position? port) (port-position port) #f))
  (define (set-position! position)
    (if (port-has-set-port-position!? port)
        (set-port-position! position port)
        #f))
  (define (close) (close-port port))
  (define (read! bv start n)
    (let loop ((count 0)
               (byte (get-u8 port)))
      (cond ((eof-object? byte) count)
            ;; XXX: consume all CRs even if not followed by LF.
            ((eqv? byte (char->integer #\return)) (loop count (get-u8 port)))
            ((= count (- n 1))
             (bytevector-u8-set! bv (+ start count) byte)
             n)
            (else
             (bytevector-u8-set! bv (+ start count) byte)
             (loop (+ count 1) (get-u8 port))))))
  (make-custom-binary-input-port "canonical-newline-port"
                                 read!
                                 get-position
                                 set-position!
                                 close))

;;;
;;; Source location.
;;;

(define absolute-dirname
  ;; Memoize to avoid repeated 'stat' storms from 'search-path'.
  (mlambda (file)
    "Return the absolute name of the directory containing FILE, or #f upon
failure."
    (match (search-path %load-path file)
      (#f #f)
      ((? string? file)
       ;; If there are relative names in %LOAD-PATH, FILE can be relative and
       ;; needs to be canonicalized.
       (if (string-prefix? "/" file)
           (dirname file)
           (canonicalize-path (dirname file)))))))

(define-syntax current-source-directory
  (lambda (s)
    "Return the absolute name of the current directory, or #f if it could not
be determined."
    (syntax-case s ()
      ((_)
       (match (assq 'filename (or (syntax-source s) '()))
         (('filename . (? string? file-name))
          ;; If %FILE-PORT-NAME-CANONICALIZATION is 'relative, then FILE-NAME
          ;; can be relative.  In that case, we try to find out at run time
          ;; the absolute file name by looking at %LOAD-PATH; doing this at
          ;; run time rather than expansion time is necessary to allow files
          ;; to be moved on the file system.
          (if (string-prefix? "/" file-name)
              (dirname file-name)
              #`(absolute-dirname #,file-name)))
         ((or ('filename . #f) #f)
          ;; raising an error would upset Geiser users
          #f))))))


;;;
;;; String comparison.
;;;

(define (string-distance s1 s2)
  "Compute the Levenshtein distance between two strings."
  ;; Naive implemenation
  (define loop
    (mlambda (as bt)
      (match as
        (() (length bt))
        ((a s ...)
         (match bt
           (() (length as))
           ((b t ...)
            (if (char=? a b)
                (loop s t)
                (1+ (min
                     (loop as t)
                     (loop s bt)
                     (loop s t))))))))))

  (let ((c1 (string->list s1))
        (c2 (string->list s2)))
    (loop c1 c2)))

(define* (string-closest trial tests #:key (threshold 3))
  "Return the string from TESTS that is the closest from the TRIAL,
according to 'string-distance'.  If the TESTS are too far from TRIAL,
according to THRESHOLD, then #f is returned."
  (identity                              ;discard second return value
    (fold2 (lambda (test closest minimal)
             (let ((dist (string-distance trial test)))
               (if (and  (< dist minimal) (< dist threshold))
                   (values test dist)
                   (values closest minimal))))
           #f +inf.0
           tests)))

;;; Local Variables:
;;; eval: (put 'call-with-progress-reporter 'scheme-indent-function 1)
;;; End:
