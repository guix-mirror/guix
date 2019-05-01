;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Jan Nieuwenhuizen <janneke@gnu.org>
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
  #:use-module (guix deprecation)
  #:use-module (guix memoization)
  #:use-module (guix serialization)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix base16)
  #:use-module (guix base32)
  #:use-module (gcrypt hash)
  #:use-module (guix profiling)
  #:autoload   (guix build syscalls) (terminal-columns)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module ((ice-9 control) #:select (let/ec))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-39)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 format)
  #:use-module (web uri)
  #:export (%daemon-socket-uri
            %gc-roots-directory
            %default-substitute-urls

            store-connection?
            store-connection-version
            store-connection-major-version
            store-connection-minor-version
            store-connection-socket

            ;; Deprecated forms for 'store-connection'.
            nix-server?
            nix-server-version
            nix-server-major-version
            nix-server-minor-version
            nix-server-socket

            current-store-protocol-version        ;for internal use
            mcached

            &store-error store-error?
            &store-connection-error store-connection-error?
            store-connection-error-file
            store-connection-error-code
            &store-protocol-error store-protocol-error?
            store-protocol-error-message
            store-protocol-error-status

            ;; Deprecated forms for '&store-error' et al.
            &nix-error nix-error?
            &nix-connection-error nix-connection-error?
            nix-connection-error-file
            nix-connection-error-code
            &nix-protocol-error nix-protocol-error?
            nix-protocol-error-message
            nix-protocol-error-status

            hash-algo
            build-mode

            open-connection
            port->connection
            close-connection
            with-store
            set-build-options
            set-build-options*
            valid-path?
            query-path-hash
            hash-part->path
            query-path-info
            add-data-to-store
            add-text-to-store
            add-to-store
            add-file-tree-to-store
            binary-file
            build-things
            build
            query-failed-paths
            clear-failed-paths
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

            path-info?
            path-info-deriver
            path-info-hash
            path-info-references
            path-info-registration-time
            path-info-nar-size

            built-in-builders
            references
            references/substitutes
            references*
            query-path-info*
            requisites
            referrers
            optimize-store
            verify-store
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

            %store-monad
            store-bind
            store-return
            store-lift
            store-lower
            run-with-store
            %guile-for-build
            current-system
            set-current-system
            text-file
            interned-file
            interned-file-tree

            %store-prefix
            store-path
            output-path
            fixed-output-path
            store-path?
            direct-store-path?
            derivation-path?
            store-path-package-name
            store-path-hash-part
            direct-store-path
            derivation-log-file
            log-file))

(define %protocol-version #x163)

(define %worker-magic-1 #x6e697863)               ; "nixc"
(define %worker-magic-2 #x6478696f)               ; "dxio"

(define (protocol-major magic)
  (logand magic #xff00))
(define (protocol-minor magic)
  (logand magic #x00ff))
(define (protocol-version major minor)
  (logior major minor))

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
  (build-things 9)
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
  (query-valid-derivers 33)
  (optimize-store 34)
  (verify-store 35)
  (built-in-builders 80))

(define-enumerate-type hash-algo
  ;; hash.hh
  (md5 1)
  (sha1 2)
  (sha256 3))

(define-enumerate-type build-mode
  ;; store-api.hh
  (normal 0)
  (repair 1)
  (check 2))

(define-enumerate-type gc-action
  ;; store-api.hh
  (return-live 0)
  (return-dead 1)
  (delete-dead 2)
  (delete-specific 3))

(define %default-socket-path
  (string-append %state-directory "/daemon-socket/socket"))

(define %daemon-socket-uri
  ;; URI or file name of the socket the daemon listens too.
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

;; Information about a store path.
(define-record-type <path-info>
  (path-info deriver hash references registration-time nar-size)
  path-info?
  (deriver path-info-deriver)                     ;string | #f
  (hash path-info-hash)
  (references path-info-references)
  (registration-time path-info-registration-time)
  (nar-size path-info-nar-size))

(define (read-path-info p)
  (let ((deriver  (match (read-store-path p)
                    ("" #f)
                    (x  x)))
        (hash     (base16-string->bytevector (read-string p)))
        (refs     (read-store-path-list p))
        (registration-time (read-int p))
        (nar-size (read-long-long p)))
    (path-info deriver hash refs registration-time nar-size)))

(define-syntax write-arg
  (syntax-rules (integer boolean bytevector
                 string string-list string-pairs
                 store-path store-path-list base16)
    ((_ integer arg p)
     (write-int arg p))
    ((_ boolean arg p)
     (write-int (if arg 1 0) p))
    ((_ bytevector arg p)
     (write-bytevector arg p))
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
  (syntax-rules (integer boolean string store-path store-path-list string-list
                 substitutable-path-list path-info base16)
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
    ((_ string-list p)
     (read-string-list p))
    ((_ substitutable-path-list p)
     (read-substitutable-path-list p))
    ((_ path-info p)
     (read-path-info p))
    ((_ base16 p)
     (base16-string->bytevector (read-string p)))))


;; remote-store.cc

(define-record-type* <store-connection> store-connection %make-store-connection
  store-connection?
  (socket store-connection-socket)
  (major  store-connection-major-version)
  (minor  store-connection-minor-version)

  (buffer store-connection-output-port)                 ;output port
  (flush  store-connection-flush-output)                ;thunk

  ;; Caches.  We keep them per-connection, because store paths build
  ;; during the session are temporary GC roots kept for the duration of
  ;; the session.
  (ats-cache    store-connection-add-to-store-cache)
  (atts-cache   store-connection-add-text-to-store-cache)
  (object-cache store-connection-object-cache
                (default vlist-null))             ;vhash
  (built-in-builders store-connection-built-in-builders
                     (default (delay '()))))      ;promise

(set-record-type-printer! <store-connection>
                          (lambda (obj port)
                            (format port "#<store-connection ~a.~a ~a>"
                                    (store-connection-major-version obj)
                                    (store-connection-minor-version obj)
                                    (number->string (object-address obj)
                                                    16))))

(define-deprecated/alias nix-server? store-connection?)
(define-deprecated/alias nix-server-major-version
  store-connection-major-version)
(define-deprecated/alias nix-server-minor-version
  store-connection-minor-version)
(define-deprecated/alias nix-server-socket store-connection-socket)


(define-condition-type &store-error &error
  store-error?)

(define-condition-type &store-connection-error &store-error
  store-connection-error?
  (file   store-connection-error-file)
  (errno  store-connection-error-code))

(define-condition-type &store-protocol-error &store-error
  store-protocol-error?
  (message store-protocol-error-message)
  (status  store-protocol-error-status))

(define-deprecated/alias &nix-error &store-error)
(define-deprecated/alias nix-error? store-error?)
(define-deprecated/alias &nix-connection-error &store-connection-error)
(define-deprecated/alias nix-connection-error? store-connection-error?)
(define-deprecated/alias nix-connection-error-file
  store-connection-error-file)
(define-deprecated/alias nix-connection-error-code
  store-connection-error-code)
(define-deprecated/alias &nix-protocol-error &store-protocol-error)
(define-deprecated/alias nix-protocol-error? store-protocol-error?)
(define-deprecated/alias nix-protocol-error-message
  store-protocol-error-message)
(define-deprecated/alias nix-protocol-error-status
  store-protocol-error-status)


(define-syntax-rule (system-error-to-connection-error file exp ...)
  "Catch 'system-error' exceptions and translate them to
'&store-connection-error'."
  (catch 'system-error
    (lambda ()
      exp ...)
    (lambda args
      (let ((errno (system-error-errno args)))
        (raise (condition (&store-connection-error
                           (file file)
                           (errno errno))))))))

(define (open-unix-domain-socket file)
  "Connect to the Unix-domain socket at FILE and return it.  Raise a
'&store-connection-error' upon error."
  (let ((s (with-fluids ((%default-port-encoding #f))
             ;; This trick allows use of the `scm_c_read' optimization.
             (socket PF_UNIX SOCK_STREAM 0)))
        (a (make-socket-address PF_UNIX file)))

    (system-error-to-connection-error file
      (connect s a)
      s)))

(define %default-guix-port
  ;; Default port when connecting to a daemon over TCP/IP.
  44146)

(define (open-inet-socket host port)
  "Connect to the Unix-domain socket at HOST:PORT and return it.  Raise a
'&store-connection-error' upon error."
  (let ((sock (with-fluids ((%default-port-encoding #f))
                ;; This trick allows use of the `scm_c_read' optimization.
                (socket PF_UNIX SOCK_STREAM 0))))
    (define addresses
      (getaddrinfo host
                   (if (number? port) (number->string port) port)
                   (if (number? port)
                       (logior AI_ADDRCONFIG AI_NUMERICSERV)
                       AI_ADDRCONFIG)
                   0                              ;any address family
                   SOCK_STREAM))                  ;TCP only

    (let loop ((addresses addresses))
      (match addresses
        ((ai rest ...)
         (let ((s (socket (addrinfo:fam ai)
                          ;; TCP/IP only
                          SOCK_STREAM IPPROTO_IP)))

           (catch 'system-error
             (lambda ()
               (connect s (addrinfo:addr ai))

               ;; Setting this option makes a dramatic difference because it
               ;; avoids the "ACK delay" on our RPC messages.
               (setsockopt s IPPROTO_TCP TCP_NODELAY 1)
               s)
             (lambda args
               ;; Connection failed, so try one of the other addresses.
               (close s)
               (if (null? rest)
                   (raise (condition (&store-connection-error
                                      (file host)
                                      (errno (system-error-errno args)))))
                   (loop rest))))))))))

(define (connect-to-daemon uri)
  "Connect to the daemon at URI, a string that may be an actual URI or a file
name."
  (define (not-supported)
    (raise (condition (&store-connection-error
                       (file uri)
                       (errno ENOTSUP)))))

  (define connect
    (match (string->uri uri)
      (#f                                         ;URI is a file name
       open-unix-domain-socket)
      ((? uri? uri)
       (match (uri-scheme uri)
         ((or #f 'file 'unix)
          (lambda (_)
            (open-unix-domain-socket (uri-path uri))))
         ('guix
          (lambda (_)
            (open-inet-socket (uri-host uri)
                              (or (uri-port uri) %default-guix-port))))
         ((? symbol? scheme)
          ;; Try to dynamically load a module for SCHEME.
          ;; XXX: Errors are swallowed.
          (match (false-if-exception
                  (resolve-interface `(guix store ,scheme)))
            ((? module? module)
             (match (false-if-exception
                     (module-ref module 'connect-to-daemon))
               ((? procedure? connect)
                (lambda (_)
                  (connect uri)))
               (x (not-supported))))
            (#f (not-supported))))
         (x
          (not-supported))))))

  (connect uri))

(define* (open-connection #:optional (uri (%daemon-socket-uri))
                          #:key port (reserve-space? #t) cpu-affinity)
  "Connect to the daemon at URI (a string), or, if PORT is not #f, use it as
the I/O port over which to communicate to a build daemon.

When RESERVE-SPACE? is true, instruct it to reserve a little bit of extra
space on the file system so that the garbage collector can still operate,
should the disk become full.  When CPU-AFFINITY is true, it must be an integer
corresponding to an OS-level CPU number to which the daemon's worker process
for this connection will be pinned.  Return a server object."
  (guard (c ((nar-error? c)
             ;; One of the 'write-' or 'read-' calls below failed, but this is
             ;; really a connection error.
             (raise (condition
                     (&store-connection-error (file (or port uri))
                                              (errno EPROTO))
                     (&message (message "build daemon handshake failed"))))))
    (let*-values (((port)
                   (or port (connect-to-daemon uri)))
                  ((output flush)
                   (buffering-output-port port
                                          (make-bytevector 8192))))
      (write-int %worker-magic-1 port)
      (let ((r (read-int port)))
        (and (eqv? r %worker-magic-2)
             (let ((v (read-int port)))
               (and (eqv? (protocol-major %protocol-version)
                          (protocol-major v))
                    (begin
                      (write-int %protocol-version port)
                      (when (>= (protocol-minor v) 14)
                        (write-int (if cpu-affinity 1 0) port)
                        (when cpu-affinity
                          (write-int cpu-affinity port)))
                      (when (>= (protocol-minor v) 11)
                        (write-int (if reserve-space? 1 0) port))
                      (letrec* ((built-in-builders
                                 (delay (%built-in-builders conn)))
                                (conn
                                 (%make-store-connection port
                                                         (protocol-major v)
                                                         (protocol-minor v)
                                                         output flush
                                                         (make-hash-table 100)
                                                         (make-hash-table 100)
                                                         vlist-null
                                                         built-in-builders)))
                        (let loop ((done? (process-stderr conn)))
                          (or done? (process-stderr conn)))
                        conn)))))))))

(define* (port->connection port
                           #:key (version %protocol-version))
  "Assimilate PORT, an input/output port, and return a connection to the
daemon, assuming the given protocol VERSION.

Warning: this procedure assumes that the initial handshake with the daemon has
already taken place on PORT and that we're just continuing on this established
connection.  Use with care."
  (let-values (((output flush)
                (buffering-output-port port (make-bytevector 8192))))
    (define connection
      (%make-store-connection port
                              (protocol-major version)
                              (protocol-minor version)
                              output flush
                              (make-hash-table 100)
                              (make-hash-table 100)
                              vlist-null
                              (delay (%built-in-builders connection))))

    connection))

(define (store-connection-version store)
  "Return the protocol version of STORE as an integer."
  (protocol-version (store-connection-major-version store)
                    (store-connection-minor-version store)))

(define-deprecated/alias nix-server-version store-connection-version)

(define (write-buffered-output server)
  "Flush SERVER's output port."
  (force-output (store-connection-output-port server))
  ((store-connection-flush-output server)))

(define (close-connection server)
  "Close the connection to SERVER."
  (close (store-connection-socket server)))

(define (call-with-store proc)
  "Call PROC with an open store connection."
  (let ((store (open-connection)))
    (dynamic-wind
      (const #f)
      (lambda ()
        (parameterize ((current-store-protocol-version
                        (store-connection-version store)))
          (proc store)))
      (lambda ()
        (false-if-exception (close-connection store))))))

(define-syntax-rule (with-store store exp ...)
  "Bind STORE to an open connection to the store and evaluate EXPs;
automatically close the store when the dynamic extent of EXP is left."
  (call-with-store (lambda (store) exp ...)))

(define current-store-protocol-version
  ;; Protocol version of the store currently used.  XXX: This is a hack to
  ;; communicate the protocol version to the build output port.  It's a hack
  ;; because it could be inaccurrate, for instance if there's code that
  ;; manipulates several store connections at once; it works well for the
  ;; purposes of (guix status) though.
  (make-parameter #f))

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
  ;; Unlike Guile's 'line, we flush upon #\return so that progress reports
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
    (store-connection-socket server))

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
             (dump-port p user-port len
                        #:buffer-size (if (<= len 16384) 16384 65536))
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
             (write-bytevector data p len)
             #f))
          ((= k %stderr-next)
           ;; Log a string.  Build logs are usually UTF-8-encoded, but they
           ;; may also contain arbitrary byte sequences that should not cause
           ;; this to fail.  Thus, use the permissive
           ;; 'read-maybe-utf8-string'.
           (let ((s (read-maybe-utf8-string p)))
             (display s (current-build-output-port))
             (when (string-any %newlines s)
               (force-output (current-build-output-port)))
             #f))
          ((= k %stderr-error)
           ;; Report an error.
           (let ((error  (read-maybe-utf8-string p))
                 ;; Currently the daemon fails to send a status code for early
                 ;; errors like DB schema version mismatches, so check for EOF.
                 (status (if (and (>= (store-connection-minor-version server) 8)
                                  (not (eof-object? (lookahead-u8 p))))
                             (read-int p)
                             1)))
             (raise (condition (&store-protocol-error
                                (message error)
                                (status  status))))))
          ((= k %stderr-last)
           ;; The daemon is done (see `stopWork' in `nix-worker.cc'.)
           #t)
          (else
           (raise (condition (&store-protocol-error
                              (message "invalid error code")
                              (status   k))))))))

(define %default-substitute-urls
  ;; Default list of substituters.  This is *not* the list baked in
  ;; 'guix-daemon', but it is used by 'guix-service-type' and and a couple of
  ;; clients ('guix build --log-file' uses it.)
  (map (if (false-if-exception (resolve-interface '(gnutls)))
           (cut string-append "https://" <>)
           (cut string-append "http://" <>))
       '("ci.guix.gnu.org")))

(define* (set-build-options server
                            #:key keep-failed? keep-going? fallback?
                            (verbosity 0)
                            rounds                ;number of build rounds
                            max-build-jobs
                            timeout
                            max-silent-time
                            (use-build-hook? #t)
                            (build-verbosity 0)
                            (log-type 0)
                            (print-build-trace #t)

                            ;; When true, provide machine-readable "build
                            ;; traces" for use by (guix status).  Old clients
                            ;; are unable to make sense, which is why it's
                            ;; disabled by default.
                            print-extended-build-trace?

                            ;; When true, the daemon prefixes builder output
                            ;; with "@ build-log" traces so we can
                            ;; distinguish it from daemon output, and we can
                            ;; distinguish each builder's output
                            ;; (PRINT-BUILD-TRACE must be true as well.)  The
                            ;; latter is particularly useful when
                            ;; MAX-BUILD-JOBS > 1.
                            multiplexed-build-output?

                            build-cores
                            (use-substitutes? #t)

                            ;; Client-provided substitute URLs.  If it is #f,
                            ;; the daemon's settings are used.  Otherwise, it
                            ;; overrides the daemons settings; see 'guix
                            ;; substitute'.
                            (substitute-urls #f)

                            ;; Number of columns in the client's terminal.
                            (terminal-columns (terminal-columns))

                            ;; Locale of the client.
                            (locale (false-if-exception (setlocale LC_ALL))))
  ;; Must be called after `open-connection'.

  (define socket
    (store-connection-socket server))

  (let-syntax ((send (syntax-rules ()
                       ((_ (type option) ...)
                        (begin
                          (write-arg type option socket)
                          ...)))))
    (write-int (operation-id set-options) socket)
    (send (boolean keep-failed?) (boolean keep-going?)
          (boolean fallback?) (integer verbosity))
    (when (< (store-connection-minor-version server) #x61)
      (let ((max-build-jobs (or max-build-jobs 1))
            (max-silent-time (or max-silent-time 3600)))
        (send (integer max-build-jobs) (integer max-silent-time))))
    (when (>= (store-connection-minor-version server) 2)
      (send (boolean use-build-hook?)))
    (when (>= (store-connection-minor-version server) 4)
      (send (integer build-verbosity) (integer log-type)
            (boolean print-build-trace)))
    (when (and (>= (store-connection-minor-version server) 6)
               (< (store-connection-minor-version server) #x61))
      (let ((build-cores (or build-cores (current-processor-count))))
        (send (integer build-cores))))
    (when (>= (store-connection-minor-version server) 10)
      (send (boolean use-substitutes?)))
    (when (>= (store-connection-minor-version server) 12)
      (let ((pairs `(;; This option is honored by 'guix substitute' et al.
                     ,@(if print-build-trace
                           `(("print-extended-build-trace"
                              . ,(if print-extended-build-trace? "1" "0")))
                           '())
                     ,@(if multiplexed-build-output?
                           `(("multiplexed-build-output"
                              . ,(if multiplexed-build-output? "true" "false")))
                           '())
                     ,@(if timeout
                           `(("build-timeout" . ,(number->string timeout)))
                           '())
                     ,@(if max-silent-time
                           `(("build-max-silent-time"
                              . ,(number->string max-silent-time)))
                           '())
                     ,@(if max-build-jobs
                           `(("build-max-jobs"
                              . ,(number->string max-build-jobs)))
                           '())
                     ,@(if build-cores
                           `(("build-cores" . ,(number->string build-cores)))
                           '())
                     ,@(if substitute-urls
                           `(("substitute-urls"
                              . ,(string-join substitute-urls)))
                           '())
                     ,@(if rounds
                           `(("build-repeat"
                              . ,(number->string (max 0 (1- rounds)))))
                           '())
                     ,@(if terminal-columns
                           `(("terminal-columns"
                              . ,(number->string terminal-columns)))
                           '())
                     ,@(if locale
                           `(("locale" . ,locale))
                           '()))))
        (send (string-pairs pairs))))
    (let loop ((done? (process-stderr server)))
      (or done? (process-stderr server)))))

(define (buffering-output-port port buffer)
  "Return two value: an output port wrapped around PORT that uses BUFFER (a
bytevector) as its internal buffer, and a thunk to flush this output port."
  ;; Note: In Guile 2.2.2, custom binary output ports already have their own
  ;; 4K internal buffer.
  (define size
    (bytevector-length buffer))

  (define total 0)

  (define (flush)
    (put-bytevector port buffer 0 total)
    (force-output port)
    (set! total 0))

  (define (write bv offset count)
    (if (zero? count)                             ;end of file
        (flush)
        (let loop ((offset offset)
                   (count count)
                   (written 0))
          (cond ((= total size)
                 (flush)
                 (loop offset count written))
                ((zero? count)
                 written)
                (else
                 (let ((to-copy (min count (- size total))))
                   (bytevector-copy! bv offset buffer total to-copy)
                   (set! total (+ total to-copy))
                   (loop (+ offset to-copy) (- count to-copy)
                         (+ written to-copy))))))))

  ;; Note: We need to return FLUSH because the custom binary port has no way
  ;; to be notified of a 'force-output' call on itself.
  (values (make-custom-binary-output-port "buffering-output-port"
                                          write #f #f flush)
          flush))

(define profiled?
  (let ((profiled
         (or (and=> (getenv "GUIX_PROFILING") string-tokenize)
             '())))
    (lambda (component)
      "Return true if COMPONENT profiling is active."
      (member component profiled))))

(define %rpc-calls
  ;; Mapping from RPC names (symbols) to invocation counts.
  (make-hash-table))

(define* (show-rpc-profile #:optional (port (current-error-port)))
  "Write to PORT a summary of the RPCs that have been made."
  (let ((profile (sort (hash-fold alist-cons '() %rpc-calls)
                       (lambda (rpc1 rpc2)
                         (< (cdr rpc1) (cdr rpc2))))))
    (format port "Remote procedure call summary: ~a RPCs~%"
            (match profile
              (((names . counts) ...)
               (reduce + 0 counts))))
    (for-each (match-lambda
                ((rpc . count)
                 (format port "  ~30a ... ~5@a~%" rpc count)))
              profile)))

(define record-operation
  ;; Optionally, increment the number of calls of the given RPC.
  (if (profiled? "rpc")
      (begin
        (register-profiling-hook! "rpc" show-rpc-profile)
        (lambda (name)
          (let ((count (or (hashq-ref %rpc-calls name) 0)))
            (hashq-set! %rpc-calls name (+ count 1)))))
      (lambda (_)
        #t)))

(define-syntax operation
  (syntax-rules ()
    "Define a client-side RPC stub for the given operation."
    ((_ (name (type arg) ...) docstring return ...)
     (lambda (server arg ...)
       docstring
       (let* ((s (store-connection-socket server))
              (buffered (store-connection-output-port server)))
         (record-operation 'name)
         (write-int (operation-id name) buffered)
         (write-arg type arg buffered)
         ...
         (write-buffered-output server)

         ;; Loop until the server is done sending error output.
         (let loop ((done? (process-stderr server)))
           (or done? (loop (process-stderr server))))
         (values (read-arg return s) ...))))))

(define-syntax-rule (define-operation (name args ...)
                      docstring return ...)
  (define name
    (operation (name args ...) docstring return ...)))

(define-operation (valid-path? (string path))
  "Return #t when PATH designates a valid store item and #f otherwise (an
invalid item may exist on disk but still be invalid, for instance because it
is the result of an aborted or failed build.)

A '&store-protocol-error' condition is raised if PATH is not prefixed by the
store directory (/gnu/store)."
  boolean)

(define-operation (query-path-hash (store-path path))
  "Return the SHA256 hash of the nar serialization of PATH as a bytevector."
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

(define-operation (query-path-info (store-path path))
  "Return the info (hash, references, etc.) for PATH."
  path-info)

(define add-data-to-store
  ;; A memoizing version of `add-to-store', to avoid repeated RPCs with
  ;; the very same arguments during a given session.
  (let ((add-text-to-store
         (operation (add-text-to-store (string name) (bytevector text)
                                       (string-list references))
                    #f
                    store-path))
        (lookup (if (profiled? "add-data-to-store-cache")
                    (let ((lookups 0)
                          (hits    0)
                          (drv     0)
                          (scheme  0))
                      (define (show-stats)
                        (define (% n)
                          (if (zero? lookups)
                              100.
                              (* 100. (/ n lookups))))

                        (format (current-error-port) "
'add-data-to-store' cache:
  lookups:      ~5@a
  hits:         ~5@a (~,1f%)
  .drv files:   ~5@a (~,1f%)
  Scheme files: ~5@a (~,1f%)~%"
                                lookups hits (% hits)
                                drv (% drv)
                                scheme (% scheme)))

                      (register-profiling-hook! "add-data-to-store-cache"
                                                show-stats)
                      (lambda (cache args)
                        (let ((result (hash-ref cache args)))
                          (set! lookups (+ 1 lookups))
                          (when result
                            (set! hits (+ 1 hits)))
                          (match args
                            ((_ name _)
                             (cond ((string-suffix? ".drv" name)
                                    (set! drv (+ drv 1)))
                                   ((string-suffix? "-builder" name)
                                    (set! scheme (+ scheme 1)))
                                   ((string-suffix? ".scm" name)
                                    (set! scheme (+ scheme 1))))))
                          result)))
                    hash-ref)))
    (lambda* (server name bytes #:optional (references '()))
      "Add BYTES under file NAME in the store, and return its store path.
REFERENCES is the list of store paths referred to by the resulting store
path."
      (let* ((args  `(,bytes ,name ,references))
             (cache (store-connection-add-text-to-store-cache server)))
        (or (lookup cache args)
            (let ((path (add-text-to-store server name bytes references)))
              (hash-set! cache args path)
              path))))))

(define* (add-text-to-store store name text #:optional (references '()))
  "Add TEXT under file NAME in the store, and return its store path.
REFERENCES is the list of store paths referred to by the resulting store
path."
  (add-data-to-store store name (string->utf8 text) references))

(define true
  ;; Define it once and for all since we use it as a default value for
  ;; 'add-to-store' and want to make sure two default values are 'eq?' for the
  ;; purposes or memoization.
  (lambda (file stat)
    #t))

(define add-to-store
  ;; A memoizing version of `add-to-store'.  This is important because
  ;; `add-to-store' leads to huge data transfers to the server, and
  ;; because it's often called many times with the very same argument.
  (let ((add-to-store
         (lambda* (server basename recursive? hash-algo file-name
                          #:key (select? true))
           ;; We don't use the 'operation' macro so we can pass SELECT? to
           ;; 'write-file'.
           (record-operation 'add-to-store)
           (let ((port (store-connection-socket server)))
             (write-int (operation-id add-to-store) port)
             (write-string basename port)
             (write-int 1 port)                   ;obsolete, must be #t
             (write-int (if recursive? 1 0) port)
             (write-string hash-algo port)
             (write-file file-name port #:select? select?)
             (write-buffered-output server)
             (let loop ((done? (process-stderr server)))
               (or done? (loop (process-stderr server))))
             (read-store-path port)))))
    (lambda* (server basename recursive? hash-algo file-name
                     #:key (select? true))
      "Add the contents of FILE-NAME under BASENAME to the store.  When
RECURSIVE? is false, FILE-NAME must designate a regular file--not a directory
nor a symlink.  When RECURSIVE? is true and FILE-NAME designates a directory,
the contents of FILE-NAME are added recursively; if FILE-NAME designates a
flat file and RECURSIVE? is true, its contents are added, and its permission
bits are kept.  HASH-ALGO must be a string such as \"sha256\".

When RECURSIVE? is true, call (SELECT?  FILE STAT) for each directory entry,
where FILE is the entry's absolute file name and STAT is the result of
'lstat'; exclude entries for which SELECT? does not return true."
      ;; Note: We don't stat FILE-NAME at each call, and thus we assume that
      ;; the file remains unchanged for the lifetime of SERVER.
      (let* ((args  `(,file-name ,basename ,recursive? ,hash-algo ,select?))
             (cache (store-connection-add-to-store-cache server)))
        (or (hash-ref cache args)
            (let ((path (add-to-store server basename recursive?
                                      hash-algo file-name
                                      #:select? select?)))
              (hash-set! cache args path)
              path))))))

(define %not-slash
  (char-set-complement (char-set #\/)))

(define* (add-file-tree-to-store server tree
                                 #:key
                                 (hash-algo "sha256")
                                 (recursive? #t))
  "Add the given TREE to the store on SERVER.  TREE must be an entry such as:

  (\"my-tree\" directory
    (\"a\" regular (data \"hello\"))
    (\"b\" symlink \"a\")
    (\"c\" directory
      (\"d\" executable (file \"/bin/sh\"))))

This is a generalized version of 'add-to-store'.  It allows you to reproduce
an arbitrary directory layout in the store without creating a derivation."

  ;; Note: The format of TREE was chosen to allow trees to be compared with
  ;; 'equal?', which in turn allows us to memoize things.

  (define root
    ;; TREE is a single entry.
    (list tree))

  (define basename
    (match tree
      ((name . _) name)))

  (define (lookup file)
    (let loop ((components (string-tokenize file %not-slash))
               (tree root))
      (match components
        ((basename)
         (assoc basename tree))
        ((head . rest)
         (loop rest
               (match (assoc-ref tree head)
                 (('directory . entries) entries)))))))

  (define (file-type+size file)
    (match (lookup file)
      ((_ (and type (or 'directory 'symlink)) . _)
       (values type 0))
      ((_ type ('file file))
       (values type (stat:size (stat file))))
      ((_ type ('data (? string? data)))
       (values type (string-length data)))
      ((_ type ('data (? bytevector? data)))
       (values type (bytevector-length data)))))

  (define (file-port file)
    (match (lookup file)
      ((_ (or 'regular 'executable) content)
       (match content
         (('file (? string? file))
          (open-file file "r0b"))
         (('data (? string? str))
          (open-input-string str))
         (('data (? bytevector? bv))
          (open-bytevector-input-port bv))))))

  (define (symlink-target file)
    (match (lookup file)
      ((_ 'symlink target) target)))

  (define (directory-entries directory)
    (match (lookup directory)
      ((_ 'directory (names . _) ...) names)))

  (define cache
    (store-connection-add-to-store-cache server))

  (or (hash-ref cache tree)
      (begin
        ;; We don't use the 'operation' macro so we can use 'write-file-tree'
        ;; instead of 'write-file'.
        (record-operation 'add-to-store/tree)
        (let ((port (store-connection-socket server)))
          (write-int (operation-id add-to-store) port)
          (write-string basename port)
          (write-int 1 port)                      ;obsolete, must be #t
          (write-int (if recursive? 1 0) port)
          (write-string hash-algo port)
          (write-file-tree basename port
                           #:file-type+size file-type+size
                           #:file-port file-port
                           #:symlink-target symlink-target
                           #:directory-entries directory-entries)
          (write-buffered-output server)
          (let loop ((done? (process-stderr server)))
            (or done? (loop (process-stderr server))))
          (let ((result (read-store-path port)))
            (hash-set! cache tree result)
            result)))))

(define build-things
  (let ((build (operation (build-things (string-list things)
                                        (integer mode))
                          "Do it!"
                          boolean))
        (build/old (operation (build-things (string-list things))
                              "Do it!"
                              boolean)))
    (lambda* (store things #:optional (mode (build-mode normal)))
      "Build THINGS, a list of store items which may be either '.drv' files or
outputs, and return when the worker is done building them.  Elements of THINGS
that are not derivations can only be substituted and not built locally.
Return #t on success."
      (parameterize ((current-store-protocol-version
                      (store-connection-version store)))
        (if (>= (store-connection-minor-version store) 15)
            (build store things mode)
            (if (= mode (build-mode normal))
                (build/old store things)
                (raise (condition (&store-protocol-error
                                   (message "unsupported build mode")
                                   (status  1))))))))))

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

(define %reference-cache
  ;; Brute-force cache mapping store items to their list of references.
  ;; Caching matters because when building a profile in the presence of
  ;; grafts, we keep calling 'graft-derivation', which in turn calls
  ;; 'references/substitutes' many times with the same arguments.  Ideally we
  ;; would use a cache associated with the daemon connection instead (XXX).
  (make-hash-table 100))

(define (references/substitutes store items)
  "Return the list of list of references of ITEMS; the result has the same
length as ITEMS.  Query substitute information for any item missing from the
store at once.  Raise a '&store-protocol-error' exception if reference
information for one of ITEMS is missing."
  (let* ((requested  items)
         (local-refs (map (lambda (item)
                            (or (hash-ref %reference-cache item)
                                (guard (c ((store-protocol-error? c) #f))
                                  (references store item))))
                          items))
         (missing    (fold-right (lambda (item local-ref result)
                                   (if local-ref
                                       result
                                       (cons item result)))
                                 '()
                                 items local-refs))

         ;; Query all the substitutes at once to minimize the cost of
         ;; launching 'guix substitute' and making HTTP requests.
         (substs     (if (null? missing)
                         '()
                         (substitutable-path-info store missing))))
    (when (< (length substs) (length missing))
      (raise (condition (&store-protocol-error
                         (message "cannot determine \
the list of references")
                         (status 1)))))

    ;; Intersperse SUBSTS and LOCAL-REFS.
    (let loop ((items       items)
               (local-refs  local-refs)
               (result      '()))
      (match items
        (()
         (let ((result (reverse result)))
           (for-each (cut hash-set! %reference-cache <> <>)
                     requested result)
           result))
        ((item items ...)
         (match local-refs
           ((#f tail ...)
            (loop items tail
                  (cons (any (lambda (subst)
                               (and (string=? (substitutable-path subst) item)
                                    (substitutable-references subst)))
                             substs)
                        result)))
           ((head tail ...)
            (loop items tail
                  (cons head result)))))))))

(define* (fold-path store proc seed paths
                    #:optional (relatives (cut references store <>)))
  "Call PROC for each of the RELATIVES of PATHS, exactly once, and return the
result formed from the successive calls to PROC, the first of which is passed
SEED."
  (let loop ((paths  paths)
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

(define (requisites store paths)
  "Return the requisites of PATHS, including PATHS---i.e., their closures (all
its references, recursively)."
  (fold-path store cons '() paths))

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
returned; thus, the resulting list can be shorter than PATHS.  Furthermore,
that there is no guarantee that the order of the resulting list matches the
order of PATHS."
             substitutable-path-list))

(define %built-in-builders
  (let ((builders (operation (built-in-builders)
                             "Return the built-in builders."
                             string-list)))
    (lambda (store)
      "Return the names of the supported built-in derivation builders
supported by STORE.  The result is memoized for STORE."
      ;; Check whether STORE's version supports this RPC and built-in
      ;; derivation builders in general, which appeared in Guix > 0.11.0.
      ;; Return the empty list if it doesn't.  Note that this RPC does not
      ;; exist in 'nix-daemon'.
      (if (or (> (store-connection-major-version store) #x100)
              (and (= (store-connection-major-version store) #x100)
                   (>= (store-connection-minor-version store) #x60)))
          (builders store)
          '()))))

(define (built-in-builders store)
  "Return the names of the supported built-in derivation builders
supported by STORE."
  (force (store-connection-built-in-builders store)))

(define-operation (optimize-store)
  "Optimize the store by hard-linking identical files (\"deduplication\".)
Return #t on success."
  ;; Note: the daemon in Guix <= 0.8.2 does not implement this RPC.
  boolean)

(define verify-store
  (let ((verify (operation (verify-store (boolean check-contents?)
                                         (boolean repair?))
                           "Verify the store."
                           boolean)))
    (lambda* (store #:key check-contents? repair?)
      "Verify the integrity of the store and return false if errors remain,
and true otherwise.  When REPAIR? is true, repair any missing or altered store
items by substituting them (this typically requires root privileges because it
is not an atomic operation.)  When CHECK-CONTENTS? is true, check the contents
of store items; this can take a lot of time."
      (not (verify store check-contents? repair?)))))

(define (run-gc server action to-delete min-freed)
  "Perform the garbage-collector operation ACTION, one of the
`gc-action' values.  When ACTION is `delete-specific', the TO-DELETE is
the list of store paths to delete.  IGNORE-LIVENESS? should always be
#f.  MIN-FREED is the minimum amount of disk space to be freed, in
bytes, before the GC can stop.  Return the list of store paths delete,
and the number of bytes freed."
  (let ((s (store-connection-socket server)))
    (write-int (operation-id collect-garbage) s)
    (write-int action s)
    (write-store-path-list to-delete s)
    (write-arg boolean #f s)                      ; ignore-liveness?
    (write-long-long min-freed s)
    (write-int 0 s)                               ; obsolete
    (when (>= (store-connection-minor-version server) 5)
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
        (hash-clear! (store-connection-add-to-store-cache server))
        (hash-clear! (store-connection-add-text-to-store-cache server)))

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
  (let ((s (store-connection-socket server)))
    (write-int (operation-id import-paths) s)
    (let loop ((done? (process-stderr server port)))
      (or done? (loop (process-stderr server port))))
    (read-store-path-list s)))

(define* (export-path server path port #:key (sign? #t))
  "Export PATH to PORT.  When SIGN? is true, sign it."
  (let ((s (store-connection-socket server)))
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

  (let loop ((paths ordered))
    (match paths
      (()
       (write-int 0 port))
      ((head tail ...)
       (write-int 1 port)
       (and (export-path server head port #:sign? sign?)
            (loop tail))))))

(define-operation (query-failed-paths)
  "Return the list of store items for which a build failure is cached.

The result is always the empty list unless the daemon was started with
'--cache-failures'."
  store-path-list)

(define-operation (clear-failed-paths (store-path-list items))
  "Remove ITEMS from the list of cached build failures.

This makes sense only when the daemon was started with '--cache-failures'."
  boolean)


;;;
;;; Store monad.
;;;

(define-syntax-rule (define-alias new old)
  (define-syntax new (identifier-syntax old)))

;; The store monad allows us to (1) build sequences of operations in the
;; store, and (2) make the store an implicit part of the execution context,
;; rather than a parameter of every single function.
(define-alias %store-monad %state-monad)
(define-alias store-return state-return)
(define-alias store-bind state-bind)

;; Instantiate templates for %STORE-MONAD since it's syntactically different
;; from %STATE-MONAD.
(template-directory instantiations %store-monad)

(define* (cache-object-mapping object keys result)
  "Augment the store's object cache with a mapping from OBJECT/KEYS to RESULT.
KEYS is a list of additional keys to match against, for instance a (SYSTEM
TARGET) tuple.

OBJECT is typically a high-level object such as a <package> or an <origin>,
and RESULT is typically its derivation."
  (lambda (store)
    (values result
            (store-connection
             (inherit store)
             (object-cache (vhash-consq object (cons result keys)
                                        (store-connection-object-cache store)))))))

(define record-cache-lookup!
  (if (profiled? "object-cache")
      (let ((fresh    0)
            (lookups  0)
            (hits     0))
        (register-profiling-hook!
         "object-cache"
         (lambda ()
           (format (current-error-port) "Store object cache:
  fresh caches: ~5@a
  lookups:      ~5@a
  hits:         ~5@a (~,1f%)~%"
                   fresh lookups hits
                   (if (zero? lookups)
                       100.
                       (* 100. (/ hits lookups))))))

        (lambda (hit? cache)
          (set! fresh
            (if (eq? cache vlist-null)
                (+ 1 fresh)
                fresh))
          (set! lookups (+ 1 lookups))
          (set! hits (if hit? (+ hits 1) hits))))
      (lambda (x y)
        #t)))

(define* (lookup-cached-object object #:optional (keys '()))
  "Return the cached object in the store connection corresponding to OBJECT
and KEYS.  KEYS is a list of additional keys to match against, and which are
compared with 'equal?'.  Return #f on failure and the cached result
otherwise."
  (lambda (store)
    (let* ((cache (store-connection-object-cache store))

           ;; Escape as soon as we find the result.  This avoids traversing
           ;; the whole vlist chain and significantly reduces the number of
           ;; 'hashq' calls.
           (value (let/ec return
                    (vhash-foldq* (lambda (item result)
                                    (match item
                                      ((value . keys*)
                                       (if (equal? keys keys*)
                                           (return value)
                                           result))))
                                  #f object
                                  cache))))
      (record-cache-lookup! value cache)
      (values value store))))

(define* (%mcached mthunk object #:optional (keys '()))
  "Bind the monadic value returned by MTHUNK, which supposedly corresponds to
OBJECT/KEYS, or return its cached value."
  (mlet %store-monad ((cached (lookup-cached-object object keys)))
    (if cached
        (return cached)
        (>>= (mthunk)
             (lambda (result)
               (cache-object-mapping object keys result))))))

(define-syntax-rule (mcached mvalue object keys ...)
  "Run MVALUE, which corresponds to OBJECT/KEYS, and cache it; or return the
value associated with OBJECT/KEYS in the store's object cache if there is
one."
  (%mcached (lambda () mvalue)
            object (list keys ...)))

(define (preserve-documentation original proc)
  "Return PROC with documentation taken from ORIGINAL."
  (set-object-property! proc 'documentation
                        (procedure-property original 'documentation))
  proc)

(define (store-lift proc)
  "Lift PROC, a procedure whose first argument is a connection to the store,
in the store monad."
  (preserve-documentation proc
                          (lambda args
                            (lambda (store)
                              (values (apply proc store args) store)))))

(define (store-lower proc)
  "Lower PROC, a monadic procedure in %STORE-MONAD, to a \"normal\" procedure
taking the store as its first argument."
  (preserve-documentation proc
                          (lambda (store . args)
                            (run-with-store store (apply proc args)))))

;;
;; Store monad operators.
;;

(define* (binary-file name
                      data ;bytevector
                      #:optional (references '()))
  "Return as a monadic value the absolute file name in the store of the file
containing DATA, a bytevector.  REFERENCES is a list of store items that the
resulting text file refers to; it defaults to the empty list."
  (lambda (store)
    (values (add-data-to-store store name data references)
            store)))

(define* (text-file name
                    text ;string
                    #:optional (references '()))
  "Return as a monadic value the absolute file name in the store of the file
containing TEXT, a string.  REFERENCES is a list of store items that the
resulting text file refers to; it defaults to the empty list."
  (lambda (store)
    (values (add-text-to-store store name text references)
            store)))

(define* (interned-file file #:optional name
                        #:key (recursive? #t) (select? true))
  "Return the name of FILE once interned in the store.  Use NAME as its store
name, or the basename of FILE if NAME is omitted.

When RECURSIVE? is true, the contents of FILE are added recursively; if FILE
designates a flat file and RECURSIVE? is true, its contents are added, and its
permission bits are kept.

When RECURSIVE? is true, call (SELECT?  FILE STAT) for each directory entry,
where FILE is the entry's absolute file name and STAT is the result of
'lstat'; exclude entries for which SELECT? does not return true."
  (lambda (store)
    (values (add-to-store store (or name (basename file))
                          recursive? "sha256" file
                          #:select? select?)
            store)))

(define interned-file-tree
  (store-lift add-file-tree-to-store))

(define build
  ;; Monadic variant of 'build-things'.
  (store-lift build-things))

(define set-build-options*
  (store-lift set-build-options))

(define references*
  (store-lift references))

(define (query-path-info* item)
  "Monadic version of 'query-path-info' that returns #f when ITEM is not in
the store."
  (lambda (store)
    (guard (c ((store-protocol-error? c)
               ;; ITEM is not in the store; return #f.
               (values #f store)))
      (values (query-path-info store item) store))))

(define-inlinable (current-system)
  ;; Consult the %CURRENT-SYSTEM fluid at bind time.  This is equivalent to
  ;; (lift0 %current-system %store-monad), but inlinable, thus avoiding
  ;; closure allocation in some cases.
  (lambda (state)
    (values (%current-system) state)))

(define-inlinable (set-current-system system)
  ;; Set the %CURRENT-SYSTEM fluid at bind time.
  (lambda (state)
    (values (%current-system system) state)))

(define %guile-for-build
  ;; The derivation of the Guile to be used within the build environment,
  ;; when using 'gexp->derivation' and co.
  (make-parameter #f))

(define* (run-with-store store mval
                         #:key
                         (guile-for-build (%guile-for-build))
                         (system (%current-system))
                         (target #f))
  "Run MVAL, a monadic value in the store monad, in STORE, an open store
connection, and return the result."
  ;; Initialize the dynamic bindings here to avoid bad surprises.  The
  ;; difficulty lies in the fact that dynamic bindings are resolved at
  ;; bind-time and not at call time, which can be disconcerting.
  (parameterize ((%guile-for-build guile-for-build)
                 (%current-system system)
                 (%current-target-system target))
    (call-with-values (lambda ()
                        (run-with-state mval store))
      (lambda (result store)
        ;; Discard the state.
        result))))


;;;
;;; Store paths.
;;;

(define %store-prefix
  ;; Absolute path to the Nix store.
  (make-parameter %store-directory))

(define (compressed-hash bv size)                 ; `compressHash'
  "Given the hash stored in BV, return a compressed version thereof that fits
in SIZE bytes."
  (define new (make-bytevector size 0))
  (define old-size (bytevector-length bv))
  (let loop ((i 0))
    (if (= i old-size)
        new
        (let* ((j (modulo i size))
               (o (bytevector-u8-ref new j)))
          (bytevector-u8-set! new j
                              (logxor o (bytevector-u8-ref bv i)))
          (loop (+ 1 i))))))

(define (store-path type hash name)               ; makeStorePath
  "Return the store path for NAME/HASH/TYPE."
  (let* ((s (string-append type ":sha256:"
                           (bytevector->base16-string hash) ":"
                           (%store-prefix) ":" name))
         (h (sha256 (string->utf8 s)))
         (c (compressed-hash h 20)))
    (string-append (%store-prefix) "/"
                   (bytevector->nix-base32-string c) "-"
                   name)))

(define (output-path output hash name)            ; makeOutputPath
  "Return an output path for OUTPUT (the name of the output as a string) of
the derivation called NAME with hash HASH."
  (store-path (string-append "output:" output) hash
              (if (string=? output "out")
                  name
                  (string-append name "-" output))))

(define* (fixed-output-path name hash
                            #:key
                            (output "out")
                            (hash-algo 'sha256)
                            (recursive? #t))
  "Return an output path for the fixed output OUTPUT defined by HASH of type
HASH-ALGO, of the derivation NAME.  RECURSIVE? has the same meaning as for
'add-to-store'."
  (if (and recursive? (eq? hash-algo 'sha256))
      (store-path "source" hash name)
      (let ((tag (string-append "fixed:" output ":"
                                (if recursive? "r:" "")
                                (symbol->string hash-algo) ":"
                                (bytevector->base16-string hash) ":")))
        (store-path (string-append "output:" output)
                    (sha256 (string->utf8 tag))
                    name))))

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

(define (direct-store-path path)
  "Return the direct store path part of PATH, stripping components after
'/gnu/store/xxxx-foo'."
  (let ((prefix-length (+ (string-length (%store-prefix)) 35)))
    (if (> (string-length path) prefix-length)
        (let ((slash (string-index path #\/ prefix-length)))
          (if slash (string-take path slash) path))
        path)))

(define (derivation-path? path)
  "Return #t if PATH is a derivation path."
  (and (store-path? path) (string-suffix? ".drv" path)))

(define store-regexp*
  ;; The substituter makes repeated calls to 'store-path-hash-part', hence
  ;; this optimization.
  (mlambda (store)
    "Return a regexp matching a file in STORE."
    (make-regexp (string-append "^" (regexp-quote store)
                                "/([0-9a-df-np-sv-z]{32})-([^/]+)$"))))

(define (store-path-package-name path)
  "Return the package name part of PATH, a file name in the store."
  (let ((path-rx (store-regexp* (%store-prefix))))
    (and=> (regexp-exec path-rx path)
           (cut match:substring <> 2))))

(define (store-path-hash-part path)
  "Return the hash part of PATH as a base32 string, or #f if PATH is not a
syntactically valid store path."
  (and (string-prefix? (%store-prefix) path)
       (let ((base (string-drop path (+ 1 (string-length (%store-prefix))))))
         (and (> (string-length base) 33)
              (let ((hash (string-take base 32)))
                (and (string-every %nix-base32-charset hash)
                     hash))))))

(define (derivation-log-file drv)
  "Return the build log file for DRV, a derivation file name, or #f if it
could not be found."
  (let* ((base    (basename drv))
         (log     (string-append (or (getenv "GUIX_LOG_DIRECTORY")
                                     (string-append %localstatedir "/log/guix"))
                                 "/drvs/"
                                 (string-take base 2) "/"
                                 (string-drop base 2)))
         (log.gz  (string-append log ".gz"))
         (log.bz2 (string-append log ".bz2")))
    (cond ((file-exists? log.gz) log.gz)
          ((file-exists? log.bz2) log.bz2)
          ((file-exists? log) log)
          (else #f))))

(define (log-file store file)
  "Return the build log file for FILE, or #f if none could be found.  FILE
must be an absolute store file name, or a derivation file name."
  (cond ((derivation-path? file)
         (derivation-log-file file))
        (else
         (match (valid-derivers store file)
           ((derivers ...)
            ;; Return the first that works.
            (any (cut log-file store <>) derivers))
           (_ #f)))))

;;; Local Variables:
;;; eval: (put 'system-error-to-connection-error 'scheme-indent-function 1)
;;; End:
