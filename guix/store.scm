;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix config)
  #:use-module (guix serialization)
  #:use-module (guix monads)
  #:autoload   (guix base32) (bytevector->base32-string)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-39)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 popen)
  #:export (%daemon-socket-file
            %gc-roots-directory

            nix-server?
            nix-server-major-version
            nix-server-minor-version
            nix-server-socket

            &nix-error nix-error?
            &nix-connection-error nix-connection-error?
            nix-connection-error-file
            nix-connection-error-code
            &nix-protocol-error nix-protocol-error?
            nix-protocol-error-message
            nix-protocol-error-status

            hash-algo

            open-connection
            close-connection
            with-store
            set-build-options
            valid-path?
            query-path-hash
            hash-part->path
            add-text-to-store
            add-to-store
            build-derivations
            add-temp-root
            add-indirect-root
            add-permanent-root
            remove-permanent-root

            substitutable?
            substitutable-path
            substitutable-deriver
            substitutable-references
            substitutable-download-size
            substitutable-nar-size
            has-substitutes?
            substitutable-paths
            substitutable-path-info

            references
            requisites
            referrers
            topologically-sorted
            valid-derivers
            query-derivation-outputs
            live-paths
            dead-paths
            collect-garbage
            delete-paths
            import-paths
            export-paths

            current-build-output-port

            register-path

            %store-monad
            store-bind
            store-return
            store-lift
            store-lower
            run-with-store
            %guile-for-build
            text-file
            interned-file

            %store-prefix
            store-path?
            direct-store-path?
            derivation-path?
            store-path-package-name
            store-path-hash-part
            log-file))

(define %protocol-version #x10c)

(define %worker-magic-1 #x6e697863)               ; "nixc"
(define %worker-magic-2 #x6478696f)               ; "dxio"

(define (protocol-major magic)
  (logand magic #xff00))
(define (protocol-minor magic)
  (logand magic #x00ff))

(define-syntax define-enumerate-type
  (syntax-rules ()
    ((_ name->int (name id) ...)
     (define-syntax name->int
       (syntax-rules (name ...)
         ((_ name) id) ...)))))

(define-enumerate-type operation-id
  ;; operation numbers from worker-protocol.hh
  (quit 0)
  (valid-path? 1)
  (has-substitutes? 3)
  (query-path-hash 4)
  (query-references 5)
  (query-referrers 6)
  (add-to-store 7)
  (add-text-to-store 8)
  (build-derivations 9)
  (ensure-path 10)
  (add-temp-root 11)
  (add-indirect-root 12)
  (sync-with-gc 13)
  (find-roots 14)
  (export-path 16)
  (query-deriver 18)
  (set-options 19)
  (collect-garbage 20)
  ;;(query-substitutable-path-info 21)  ; obsolete as of #x10c
  (query-derivation-outputs 22)
  (query-all-valid-paths 23)
  (query-failed-paths 24)
  (clear-failed-paths 25)
  (query-path-info 26)
  (import-paths 27)
  (query-derivation-output-names 28)
  (query-path-from-hash-part 29)
  (query-substitutable-path-infos 30)
  (query-valid-paths 31)
  (query-substitutable-paths 32)
  (query-valid-derivers 33))

(define-enumerate-type hash-algo
  ;; hash.hh
  (md5 1)
  (sha1 2)
  (sha256 3))

(define-enumerate-type gc-action
  ;; store-api.hh
  (return-live 0)
  (return-dead 1)
  (delete-dead 2)
  (delete-specific 3))

(define %default-socket-path
  (string-append %state-directory "/daemon-socket/socket"))

(define %daemon-socket-file
  ;; File name of the socket the daemon listens too.
  (make-parameter (or (getenv "GUIX_DAEMON_SOCKET")
                      %default-socket-path)))



;; Information about a substitutable store path.
(define-record-type <substitutable>
  (substitutable path deriver refs dl-size nar-size)
  substitutable?
  (path      substitutable-path)
  (deriver   substitutable-deriver)
  (refs      substitutable-references)
  (dl-size   substitutable-download-size)
  (nar-size  substitutable-nar-size))

(define (read-substitutable-path-list p)
  (let loop ((len    (read-int p))
             (result '()))
    (if (zero? len)
        (reverse result)
        (let ((path     (read-store-path p))
              (deriver  (read-store-path p))
              (refs     (read-store-path-list p))
              (dl-size  (read-long-long p))
              (nar-size (read-long-long p)))
          (loop (- len 1)
                (cons (substitutable path deriver refs dl-size nar-size)
                      result))))))

(define-syntax write-arg
  (syntax-rules (integer boolean file string string-list string-pairs
                 store-path store-path-list base16)
    ((_ integer arg p)
     (write-int arg p))
    ((_ boolean arg p)
     (write-int (if arg 1 0) p))
    ((_ file arg p)
     (write-file arg p))
    ((_ string arg p)
     (write-string arg p))
    ((_ string-list arg p)
     (write-string-list arg p))
    ((_ string-pairs arg p)
     (write-string-pairs arg p))
    ((_ store-path arg p)
     (write-store-path arg p))
    ((_ store-path-list arg p)
     (write-store-path-list arg p))
    ((_ base16 arg p)
     (write-string (bytevector->base16-string arg) p))))

(define-syntax read-arg
  (syntax-rules (integer boolean string store-path store-path-list
                 substitutable-path-list base16)
    ((_ integer p)
     (read-int p))
    ((_ boolean p)
     (not (zero? (read-int p))))
    ((_ string p)
     (read-string p))
    ((_ store-path p)
     (read-store-path p))
    ((_ store-path-list p)
     (read-store-path-list p))
    ((_ substitutable-path-list p)
     (read-substitutable-path-list p))
    ((_ base16 p)
     (base16-string->bytevector (read-string p)))))


;; remote-store.cc

(define-record-type <nix-server>
  (%make-nix-server socket major minor
                    ats-cache atts-cache)
  nix-server?
  (socket nix-server-socket)
  (major  nix-server-major-version)
  (minor  nix-server-minor-version)

  ;; Caches.  We keep them per-connection, because store paths build
  ;; during the session are temporary GC roots kept for the duration of
  ;; the session.
  (ats-cache  nix-server-add-to-store-cache)
  (atts-cache nix-server-add-text-to-store-cache))

(set-record-type-printer! <nix-server>
                          (lambda (obj port)
                            (format port "#<build-daemon ~a.~a ~a>"
                                    (nix-server-major-version obj)
                                    (nix-server-minor-version obj)
                                    (number->string (object-address obj)
                                                    16))))

(define-condition-type &nix-error &error
  nix-error?)

(define-condition-type &nix-connection-error &nix-error
  nix-connection-error?
  (file   nix-connection-error-file)
  (errno  nix-connection-error-code))

(define-condition-type &nix-protocol-error &nix-error
  nix-protocol-error?
  (message nix-protocol-error-message)
  (status  nix-protocol-error-status))

(define* (open-connection #:optional (file (%daemon-socket-file))
                          #:key (reserve-space? #t))
  "Connect to the daemon over the Unix-domain socket at FILE.  When
RESERVE-SPACE? is true, instruct it to reserve a little bit of extra
space on the file system so that the garbage collector can still
operate, should the disk become full.  Return a server object."
  (let ((s (with-fluids ((%default-port-encoding #f))
             ;; This trick allows use of the `scm_c_read' optimization.
             (socket PF_UNIX SOCK_STREAM 0)))
        (a (make-socket-address PF_UNIX file)))

    (catch 'system-error
      (cut connect s a)
      (lambda args
        ;; Translate the error to something user-friendly.
        (let ((errno (system-error-errno args)))
          (raise (condition (&nix-connection-error
                             (file file)
                             (errno errno)))))))

    (write-int %worker-magic-1 s)
    (let ((r (read-int s)))
      (and (eqv? r %worker-magic-2)
           (let ((v (read-int s)))
             (and (eqv? (protocol-major %protocol-version)
                        (protocol-major v))
                  (begin
                    (write-int %protocol-version s)
                    (if (>= (protocol-minor v) 11)
                        (write-int (if reserve-space? 1 0) s))
                    (let ((s (%make-nix-server s
                                               (protocol-major v)
                                               (protocol-minor v)
                                               (make-hash-table 100)
                                               (make-hash-table 100))))
                      (let loop ((done? (process-stderr s)))
                        (or done? (process-stderr s)))
                      s))))))))

(define (close-connection server)
  "Close the connection to SERVER."
  (close (nix-server-socket server)))

(define-syntax-rule (with-store store exp ...)
  "Bind STORE to an open connection to the store and evaluate EXPs;
automatically close the store when the dynamic extent of EXP is left."
  (let ((store (open-connection)))
    (dynamic-wind
      (const #f)
      (lambda ()
        exp ...)
      (lambda ()
        (false-if-exception (close-connection store))))))

(define current-build-output-port
  ;; The port where build output is sent.
  (make-parameter (current-error-port)))

(define* (dump-port in out
                    #:optional len
                    #:key (buffer-size 16384))
  "Read LEN bytes from IN (or as much as possible if LEN is #f) and write it
to OUT, using chunks of BUFFER-SIZE bytes."
  (define buffer
    (make-bytevector buffer-size))

  (let loop ((total 0)
             (bytes (get-bytevector-n! in buffer 0
                                       (if len
                                           (min len buffer-size)
                                           buffer-size))))
    (or (eof-object? bytes)
        (and len (= total len))
        (let ((total (+ total bytes)))
          (put-bytevector out buffer 0 bytes)
          (loop total
                (get-bytevector-n! in buffer 0
                                   (if len
                                       (min (- len total) buffer-size)
                                       buffer-size)))))))

(define %newlines
  ;; Newline characters triggering a flush of 'current-build-output-port'.
  ;; Unlike Guile's _IOLBF, we flush upon #\return so that progress reports
  ;; that use that trick are correctly displayed.
  (char-set #\newline #\return))

(define* (process-stderr server #:optional user-port)
  "Read standard output and standard error from SERVER, writing it to
CURRENT-BUILD-OUTPUT-PORT.  Return #t when SERVER is done sending data, and
#f otherwise; in the latter case, the caller should call `process-stderr'
again until #t is returned or an error is raised.

Since the build process's output cannot be assumed to be UTF-8, we
conservatively consider it to be Latin-1, thereby avoiding possible
encoding conversion errors."
  (define p
    (nix-server-socket server))

  ;; magic cookies from worker-protocol.hh
  (define %stderr-next  #x6f6c6d67)          ; "olmg", build log
  (define %stderr-read  #x64617461)          ; "data", data needed from source
  (define %stderr-write #x64617416)          ; "dat\x16", data for sink
  (define %stderr-last  #x616c7473)          ; "alts", we're done
  (define %stderr-error #x63787470)          ; "cxtp", error reporting

  (let ((k (read-int p)))
    (cond ((= k %stderr-write)
           ;; Write a byte stream to USER-PORT.
           (let* ((len (read-int p))
                  (m   (modulo len 8)))
             (dump-port p user-port len)
             (unless (zero? m)
               ;; Consume padding, as for strings.
               (get-bytevector-n p (- 8 m))))
           #f)
          ((= k %stderr-read)
           ;; Read a byte stream from USER-PORT.
           ;; Note: Avoid 'get-bytevector-n' to work around
           ;; <http://bugs.gnu.org/17591> in Guile up to 2.0.11.
           (let* ((max-len (read-int p))
                  (data    (make-bytevector max-len))
                  (len     (get-bytevector-n! user-port data 0 max-len)))
             (write-int len p)
             (put-bytevector p data 0 len)
             (write-padding len p)
             #f))
          ((= k %stderr-next)
           ;; Log a string.
           (let ((s (read-latin1-string p)))
             (display s (current-build-output-port))
             (when (string-any %newlines s)
               (flush-output-port (current-build-output-port)))
             #f))
          ((= k %stderr-error)
           ;; Report an error.
           (let ((error  (read-latin1-string p))
                 ;; Currently the daemon fails to send a status code for early
                 ;; errors like DB schema version mismatches, so check for EOF.
                 (status (if (and (>= (nix-server-minor-version server) 8)
                                  (not (eof-object? (lookahead-u8 p))))
                             (read-int p)
                             1)))
             (raise (condition (&nix-protocol-error
                                (message error)
                                (status  status))))))
          ((= k %stderr-last)
           ;; The daemon is done (see `stopWork' in `nix-worker.cc'.)
           #t)
          (else
           (raise (condition (&nix-protocol-error
                              (message "invalid error code")
                              (status   k))))))))

(define* (set-build-options server
                            #:key keep-failed? keep-going? fallback?
                            (verbosity 0)
                            (max-build-jobs 1)
                            timeout
                            (max-silent-time 3600)
                            (use-build-hook? #t)
                            (build-verbosity 0)
                            (log-type 0)
                            (print-build-trace #t)
                            (build-cores (current-processor-count))
                            (use-substitutes? #t)
                            (binary-caches '())) ; client "untrusted" cache URLs
  ;; Must be called after `open-connection'.

  (define socket
    (nix-server-socket server))

  (let-syntax ((send (syntax-rules ()
                       ((_ (type option) ...)
                        (begin
                          (write-arg type option socket)
                          ...)))))
    (write-int (operation-id set-options) socket)
    (send (boolean keep-failed?) (boolean keep-going?)
          (boolean fallback?) (integer verbosity)
          (integer max-build-jobs) (integer max-silent-time))
    (when (>= (nix-server-minor-version server) 2)
      (send (boolean use-build-hook?)))
    (when (>= (nix-server-minor-version server) 4)
      (send (integer build-verbosity) (integer log-type)
            (boolean print-build-trace)))
    (when (>= (nix-server-minor-version server) 6)
      (send (integer build-cores)))
    (when (>= (nix-server-minor-version server) 10)
      (send (boolean use-substitutes?)))
    (when (>= (nix-server-minor-version server) 12)
      (let ((pairs (if timeout
                       `(("build-timeout" . ,(number->string timeout))
                         ,@binary-caches)
                       binary-caches)))
        (send (string-pairs pairs))))
    (let loop ((done? (process-stderr server)))
      (or done? (process-stderr server)))))

(define-syntax operation
  (syntax-rules ()
    "Define a client-side RPC stub for the given operation."
    ((_ (name (type arg) ...) docstring return ...)
     (lambda (server arg ...)
       docstring
       (let ((s (nix-server-socket server)))
         (write-int (operation-id name) s)
         (write-arg type arg s)
         ...
         ;; Loop until the server is done sending error output.
         (let loop ((done? (process-stderr server)))
           (or done? (loop (process-stderr server))))
         (values (read-arg return s) ...))))))

(define-syntax-rule (define-operation (name args ...)
                      docstring return ...)
  (define name
    (operation (name args ...) docstring return ...)))

(define-operation (valid-path? (string path))
  "Return #t when PATH is a valid store path."
  boolean)

(define-operation (query-path-hash (store-path path))
  "Return the SHA256 hash of PATH as a bytevector."
  base16)

(define hash-part->path
  (let ((query-path-from-hash-part
         (operation (query-path-from-hash-part (string hash))
                    #f
                    store-path)))
   (lambda (server hash-part)
     "Return the store path whose hash part is HASH-PART (a nix-base32
string).  Raise an error if no such path exists."
     ;; This RPC is primarily used by Hydra to reply to HTTP GETs of
     ;; /HASH.narinfo.
     (query-path-from-hash-part server hash-part))))

(define add-text-to-store
  ;; A memoizing version of `add-to-store', to avoid repeated RPCs with
  ;; the very same arguments during a given session.
  (let ((add-text-to-store
         (operation (add-text-to-store (string name) (string text)
                                       (string-list references))
                    #f
                    store-path)))
    (lambda* (server name text #:optional (references '()))
      "Add TEXT under file NAME in the store, and return its store path.
REFERENCES is the list of store paths referred to by the resulting store
path."
      (let ((args  `(,text ,name ,references))
            (cache (nix-server-add-text-to-store-cache server)))
        (or (hash-ref cache args)
            (let ((path (add-text-to-store server name text references)))
              (hash-set! cache args path)
              path))))))

(define add-to-store
  ;; A memoizing version of `add-to-store'.  This is important because
  ;; `add-to-store' leads to huge data transfers to the server, and
  ;; because it's often called many times with the very same argument.
  (let ((add-to-store (operation (add-to-store (string basename)
                                               (boolean fixed?) ; obsolete, must be #t
                                               (boolean recursive?)
                                               (string hash-algo)
                                               (file file-name))
                                 #f
                                 store-path)))
    (lambda (server basename recursive? hash-algo file-name)
      "Add the contents of FILE-NAME under BASENAME to the store.  When
RECURSIVE? is true and FILE-NAME designates a directory, the contents of
FILE-NAME are added recursively; if FILE-NAME designates a flat file and
RECURSIVE? is true, its contents are added, and its permission bits are
kept.  HASH-ALGO must be a string such as \"sha256\"."
      (let* ((st    (stat file-name #f))
             (args  `(,st ,basename ,recursive? ,hash-algo))
             (cache (nix-server-add-to-store-cache server)))
        (or (and st (hash-ref cache args))
            (let ((path (add-to-store server basename #t recursive?
                                      hash-algo file-name)))
              (hash-set! cache args path)
              path))))))

(define-operation (build-derivations (string-list derivations))
  "Build DERIVATIONS, and return when the worker is done building them.
Return #t on success."
  boolean)

(define-operation (add-temp-root (store-path path))
  "Make PATH a temporary root for the duration of the current session.
Return #t."
  boolean)

(define-operation (add-indirect-root (string file-name))
  "Make the symlink FILE-NAME an indirect root for the garbage collector:
whatever store item FILE-NAME points to will not be collected.  Return #t on
success.

FILE-NAME can be anywhere on the file system, but it must be an absolute file
name--it is the caller's responsibility to ensure that it is an absolute file
name."
  boolean)

(define %gc-roots-directory
  ;; The place where garbage collector roots (symlinks) are kept.
  (string-append %state-directory "/gcroots"))

(define (add-permanent-root target)
  "Add a garbage collector root pointing to TARGET, an element of the store,
preventing TARGET from even being collected.  This can also be used if TARGET
does not exist yet.

Raise an error if the caller does not have write access to the GC root
directory."
  (let* ((root (string-append %gc-roots-directory "/" (basename target))))
    (catch 'system-error
      (lambda ()
        (symlink target root))
      (lambda args
        ;; If ROOT already exists, this is fine; otherwise, re-throw.
        (unless (= EEXIST (system-error-errno args))
          (apply throw args))))))

(define (remove-permanent-root target)
  "Remove the permanent garbage collector root pointing to TARGET.  Raise an
error if there is no such root."
  (delete-file (string-append %gc-roots-directory "/" (basename target))))

(define references
  (operation (query-references (store-path path))
             "Return the list of references of PATH."
             store-path-list))

(define* (fold-path store proc seed path
                    #:optional (relatives (cut references store <>)))
  "Call PROC for each of the RELATIVES of PATH, exactly once, and return the
result formed from the successive calls to PROC, the first of which is passed
SEED."
  (let loop ((paths  (list path))
             (result seed)
             (seen   vlist-null))
    (match paths
      ((path rest ...)
       (if (vhash-assoc path seen)
           (loop rest result seen)
           (let ((seen   (vhash-cons path #t seen))
                 (rest   (append rest (relatives path)))
                 (result (proc path result)))
             (loop rest result seen))))
      (()
       result))))

(define (requisites store path)
  "Return the requisites of PATH, including PATH---i.e., its closure (all its
references, recursively)."
  (fold-path store cons '() path))

(define (topologically-sorted store paths)
  "Return a list containing PATHS and all their references sorted in
topological order."
  (define (traverse)
    ;; Do a simple depth-first traversal of all of PATHS.
    (let loop ((paths   paths)
               (visited vlist-null)
               (result  '()))
      (define (visit n)
        (vhash-cons n #t visited))

      (define (visited? n)
        (vhash-assoc n visited))

      (match paths
        ((head tail ...)
         (if (visited? head)
             (loop tail visited result)
             (call-with-values
                 (lambda ()
                   (loop (references store head)
                         (visit head)
                         result))
               (lambda (visited result)
                 (loop tail
                       visited
                       (cons head result))))))
        (()
         (values visited result)))))

  (call-with-values traverse
    (lambda (_ result)
      (reverse result))))

(define referrers
  (operation (query-referrers (store-path path))
             "Return the list of path that refer to PATH."
             store-path-list))

(define valid-derivers
  (operation (query-valid-derivers (store-path path))
             "Return the list of valid \"derivers\" of PATH---i.e., all the
.drv present in the store that have PATH among their outputs."
             store-path-list))

(define query-derivation-outputs  ; avoid name clash with `derivation-outputs'
  (operation (query-derivation-outputs (store-path path))
             "Return the list of outputs of PATH, a .drv file."
             store-path-list))

(define-operation (has-substitutes? (store-path path))
  "Return #t if binary substitutes are available for PATH, and #f otherwise."
  boolean)

(define substitutable-paths
  (operation (query-substitutable-paths (store-path-list paths))
             "Return the subset of PATHS that is substitutable."
             store-path-list))

(define substitutable-path-info
  (operation (query-substitutable-path-infos (store-path-list paths))
             "Return information about the subset of PATHS that is
substitutable.  For each substitutable path, a `substitutable?' object is
returned."
             substitutable-path-list))

(define (run-gc server action to-delete min-freed)
  "Perform the garbage-collector operation ACTION, one of the
`gc-action' values.  When ACTION is `delete-specific', the TO-DELETE is
the list of store paths to delete.  IGNORE-LIVENESS? should always be
#f.  MIN-FREED is the minimum amount of disk space to be freed, in
bytes, before the GC can stop.  Return the list of store paths delete,
and the number of bytes freed."
  (let ((s (nix-server-socket server)))
    (write-int (operation-id collect-garbage) s)
    (write-int action s)
    (write-store-path-list to-delete s)
    (write-arg boolean #f s)                      ; ignore-liveness?
    (write-long-long min-freed s)
    (write-int 0 s)                               ; obsolete
    (when (>= (nix-server-minor-version server) 5)
      ;; Obsolete `use-atime' and `max-atime' parameters.
      (write-int 0 s)
      (write-int 0 s))

    ;; Loop until the server is done sending error output.
    (let loop ((done? (process-stderr server)))
      (or done? (loop (process-stderr server))))

    (let ((paths    (read-store-path-list s))
          (freed    (read-long-long s))
          (obsolete (read-long-long s)))
      (unless (null? paths)
        ;; To be on the safe side, completely invalidate both caches.
        ;; Otherwise we could end up returning store paths that are no longer
        ;; valid.
        (hash-clear! (nix-server-add-to-store-cache server))
        (hash-clear! (nix-server-add-text-to-store-cache server)))

     (values paths freed))))

(define-syntax-rule (%long-long-max)
  ;; Maximum unsigned 64-bit integer.
  (- (expt 2 64) 1))

(define (live-paths server)
  "Return the list of live store paths---i.e., store paths still
referenced, and thus not subject to being garbage-collected."
  (run-gc server (gc-action return-live) '() (%long-long-max)))

(define (dead-paths server)
  "Return the list of dead store paths---i.e., store paths no longer
referenced, and thus subject to being garbage-collected."
  (run-gc server (gc-action return-dead) '() (%long-long-max)))

(define* (collect-garbage server #:optional (min-freed (%long-long-max)))
  "Collect garbage from the store at SERVER.  If MIN-FREED is non-zero,
then collect at least MIN-FREED bytes.  Return the paths that were
collected, and the number of bytes freed."
  (run-gc server (gc-action delete-dead) '() min-freed))

(define* (delete-paths server paths #:optional (min-freed (%long-long-max)))
  "Delete PATHS from the store at SERVER, if they are no longer
referenced.  If MIN-FREED is non-zero, then stop after at least
MIN-FREED bytes have been collected.  Return the paths that were
collected, and the number of bytes freed."
  (run-gc server (gc-action delete-specific) paths min-freed))

(define (import-paths server port)
  "Import the set of store paths read from PORT into SERVER's store.  An error
is raised if the set of paths read from PORT is not signed (as per
'export-path #:sign? #t'.)  Return the list of store paths imported."
  (let ((s (nix-server-socket server)))
    (write-int (operation-id import-paths) s)
    (let loop ((done? (process-stderr server port)))
      (or done? (loop (process-stderr server port))))
    (read-store-path-list s)))

(define* (export-path server path port #:key (sign? #t))
  "Export PATH to PORT.  When SIGN? is true, sign it."
  (let ((s (nix-server-socket server)))
    (write-int (operation-id export-path) s)
    (write-store-path path s)
    (write-arg boolean sign? s)
    (let loop ((done? (process-stderr server port)))
      (or done? (loop (process-stderr server port))))
    (= 1 (read-int s))))

(define* (export-paths server paths port #:key (sign? #t) recursive?)
  "Export the store paths listed in PATHS to PORT, in topological order,
signing them if SIGN? is true.  When RECURSIVE? is true, export the closure of
PATHS---i.e., PATHS and all their dependencies."
  (define ordered
    (let ((sorted (topologically-sorted server paths)))
      ;; When RECURSIVE? is #f, filter out the references of PATHS.
      (if recursive?
          sorted
          (filter (cut member <> paths) sorted))))

  (let ((s (nix-server-socket server)))
    (let loop ((paths ordered))
      (match paths
        (()
         (write-int 0 port))
        ((head tail ...)
         (write-int 1 port)
         (and (export-path server head port #:sign? sign?)
              (loop tail)))))))

(define* (register-path path
                        #:key (references '()) deriver prefix
                        state-directory)
  "Register PATH as a valid store file, with REFERENCES as its list of
references, and DERIVER as its deriver (.drv that led to it.)  If PREFIX is
not #f, it must be the name of the directory containing the new store to
initialize; if STATE-DIRECTORY is not #f, it must be a string containing the
absolute file name to the state directory of the store being initialized.
Return #t on success.

Use with care as it directly modifies the store!  This is primarily meant to
be used internally by the daemon's build hook."
  ;; Currently this is implemented by calling out to the fine C++ blob.
  (catch 'system-error
    (lambda ()
      (let ((pipe (apply open-pipe* OPEN_WRITE %guix-register-program
                         `(,@(if prefix
                                 `("--prefix" ,prefix)
                                 '())
                           ,@(if state-directory
                                 `("--state-directory" ,state-directory)
                                 '())))))
        (and pipe
             (begin
               (format pipe "~a~%~a~%~a~%"
                       path (or deriver "") (length references))
               (for-each (cut format pipe "~a~%" <>) references)
               (zero? (close-pipe pipe))))))
    (lambda args
      ;; Failed to run %GUIX-REGISTER-PROGRAM.
      #f)))


;;;
;;; Store monad.
;;;

;; return:: a -> StoreM a
(define-inlinable (store-return value)
  "Return VALUE from a monadic function."
  ;; The monadic value is just this.
  (lambda (store)
    value))

;; >>=:: StoreM a -> (a -> StoreM b) -> StoreM b
(define-inlinable (store-bind mvalue mproc)
  "Bind MVALUE in MPROC."
  (lambda (store)
    (let* ((value   (mvalue store))
           (mresult (mproc value)))
      (mresult store))))

;; This is essentially a state monad
(define-monad %store-monad
  (bind   store-bind)
  (return store-return))

(define (store-lift proc)
  "Lift PROC, a procedure whose first argument is a connection to the store,
in the store monad."
  (define result
    (lambda args
      (lambda (store)
        (apply proc store args))))

  (set-object-property! result 'documentation
                        (procedure-property proc 'documentation))
  result)

(define (store-lower proc)
  "Lower PROC, a monadic procedure in %STORE-MONAD, to a \"normal\" procedure
taking the store as its first argument."
  (lambda (store . args)
    (run-with-store store (apply proc args))))

;;
;; Store monad operators.
;;

(define* (text-file name text)
  "Return as a monadic value the absolute file name in the store of the file
containing TEXT, a string."
  (lambda (store)
    (add-text-to-store store name text '())))

(define* (interned-file file #:optional name
                        #:key (recursive? #t))
  "Return the name of FILE once interned in the store.  Use NAME as its store
name, or the basename of FILE if NAME is omitted.

When RECURSIVE? is true, the contents of FILE are added recursively; if FILE
designates a flat file and RECURSIVE? is true, its contents are added, and its
permission bits are kept."
  (lambda (store)
    (add-to-store store (or name (basename file))
                  recursive? "sha256" file)))

(define %guile-for-build
  ;; The derivation of the Guile to be used within the build environment,
  ;; when using 'gexp->derivation' and co.
  (make-parameter #f))

(define* (run-with-store store mval
                         #:key
                         (guile-for-build (%guile-for-build))
                         (system (%current-system)))
  "Run MVAL, a monadic value in the store monad, in STORE, an open store
connection."
  (parameterize ((%guile-for-build guile-for-build)
                 (%current-system system))
    (mval store)))


;;;
;;; Store paths.
;;;

(define %store-prefix
  ;; Absolute path to the Nix store.
  (make-parameter %store-directory))

(define (store-path? path)
  "Return #t if PATH is a store path."
  ;; This is a lightweight check, compared to using a regexp, but this has to
  ;; be fast as it's called often in `derivation', for instance.
  ;; `isStorePath' in Nix does something similar.
  (string-prefix? (%store-prefix) path))

(define (direct-store-path? path)
  "Return #t if PATH is a store path, and not a sub-directory of a store path.
This predicate is sometimes needed because files *under* a store path are not
valid inputs."
  (and (store-path? path)
       (not (string=? path (%store-prefix)))
       (let ((len (+ 1 (string-length (%store-prefix)))))
         (not (string-index (substring path len) #\/)))))

(define (derivation-path? path)
  "Return #t if PATH is a derivation path."
  (and (store-path? path) (string-suffix? ".drv" path)))

(define store-regexp*
  ;; The substituter makes repeated calls to 'store-path-hash-part', hence
  ;; this optimization.
  (memoize
   (lambda (store)
     "Return a regexp matching a file in STORE."
     (make-regexp (string-append "^" (regexp-quote store)
                                 "/([0-9a-df-np-sv-z]{32})-([^/]+)$")))))

(define (store-path-package-name path)
  "Return the package name part of PATH, a file name in the store."
  (let ((path-rx (store-regexp* (%store-prefix))))
    (and=> (regexp-exec path-rx path)
           (cut match:substring <> 2))))

(define (store-path-hash-part path)
  "Return the hash part of PATH as a base32 string, or #f if PATH is not a
syntactically valid store path."
  (let ((path-rx (store-regexp* (%store-prefix))))
    (and=> (regexp-exec path-rx path)
           (cut match:substring <> 1))))

(define (log-file store file)
  "Return the build log file for FILE, or #f if none could be found.  FILE
must be an absolute store file name, or a derivation file name."
  (cond ((derivation-path? file)
         (let* ((base    (basename file))
                (log     (string-append (dirname %state-directory) ; XXX
                                        "/log/guix/drvs/"
                                        (string-take base 2) "/"
                                        (string-drop base 2)))
                (log.bz2 (string-append log ".bz2")))
           (cond ((file-exists? log.bz2) log.bz2)
                 ((file-exists? log) log)
                 (else #f))))
        (else
         (match (valid-derivers store file)
           ((derivers ...)
            ;; Return the first that works.
            (any (cut log-file store <>) derivers))
           (_ #f)))))
