;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts publish)
  #:use-module ((system repl server) #:prefix repl:)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (web http)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri)
  #:use-module (guix base32)
  #:use-module (guix base64)
  #:use-module (guix config)
  #:use-module (guix derivations)
  #:use-module (guix hash)
  #:use-module (guix pki)
  #:use-module (guix pk-crypto)
  #:use-module (guix store)
  #:use-module (guix serialization)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:export (guix-publish))

(define (show-help)
  (format #t (_ "Usage: guix publish [OPTION]...
Publish ~a over HTTP.\n") %store-directory)
  (display (_ "
  -p, --port=PORT        listen on PORT"))
  (display (_ "
      --listen=HOST      listen on the network interface for HOST"))
  (display (_ "
  -u, --user=USER        change privileges to USER as soon as possible"))
  (display (_ "
  -r, --repl[=PORT]      spawn REPL server on PORT"))
  (newline)
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define (getaddrinfo* host)
  "Like 'getaddrinfo', but properly report errors."
  (catch 'getaddrinfo-error
    (lambda ()
      (getaddrinfo host))
    (lambda (key error)
      (leave (_ "lookup of host '~a' failed: ~a~%")
             host (gai-strerror error)))))

(define %options
  (list (option '(#\h "help") #f #f
                (lambda _
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda _
                  (show-version-and-exit "guix publish")))
        (option '(#\u "user") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'user arg result)))
        (option '(#\p "port") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'port (string->number* arg) result)))
        (option '("listen") #t #f
                (lambda (opt name arg result)
                  (match (getaddrinfo* arg)
                    ((info _ ...)
                     (alist-cons 'address (addrinfo:addr info)
                                 result))
                    (()
                     (leave (_ "lookup of host '~a' returned nothing")
                            name)))))
        (option '(#\r "repl") #f #t
                (lambda (opt name arg result)
                  ;; If port unspecified, use default Guile REPL port.
                  (let ((port (and arg (string->number* arg))))
                    (alist-cons 'repl (or port 37146) result))))))

(define %default-options
  `((port . 8080)
    (address . ,(make-socket-address AF_INET INADDR_ANY 0))
    (repl . #f)))

(define (lazy-read-file-sexp file)
  "Return a promise to read the canonical sexp from FILE."
  (delay
    (call-with-input-file file
      (compose string->canonical-sexp
               get-string-all))))

(define %private-key
  (lazy-read-file-sexp %private-key-file))

(define %public-key
  (lazy-read-file-sexp %public-key-file))

(define %nix-cache-info
  `(("StoreDir" . ,%store-directory)
    ("WantMassQuery" . 0)
    ("Priority" . 100)))

(define (load-derivation file)
  "Read the derivation from FILE."
  (call-with-input-file file read-derivation))

(define (signed-string s)
  "Sign the hash of the string S with the daemon's key."
  (let* ((public-key (force %public-key))
         (hash (bytevector->hash-data (sha256 (string->utf8 s))
                                      #:key-type (key-type public-key))))
    (signature-sexp hash (force %private-key) public-key)))

(define base64-encode-string
  (compose base64-encode string->utf8))

(define (narinfo-string store store-path key)
  "Generate a narinfo key/value string for STORE-PATH; an exception is raised
if STORE-PATH is invalid.  The narinfo is signed with KEY."
  (let* ((path-info  (query-path-info store store-path))
         (url        (string-append "nar/" (basename store-path)))
         (hash       (bytevector->nix-base32-string
                      (path-info-hash path-info)))
         (size       (path-info-nar-size path-info))
         (references (string-join
                      (map basename (path-info-references path-info))
                      " "))
         (deriver    (path-info-deriver path-info))
         (base-info  (format #f
                             "StorePath: ~a
URL: ~a
Compression: none
NarHash: sha256:~a
NarSize: ~d
References: ~a~%"
                             store-path url hash size references))
         ;; Do not render a "Deriver" or "System" line if we are rendering
         ;; info for a derivation.
         (info       (if (not deriver)
                         base-info
                         (catch 'system-error
                           (lambda ()
                             (let ((drv (load-derivation deriver)))
                               (format #f "~aSystem: ~a~%Deriver: ~a~%"
                                       base-info (derivation-system drv)
                                       (basename deriver))))
                           (lambda args
                             ;; DERIVER might be missing, but that's fine:
                             ;; it's only used for <substitutable> where it's
                             ;; optional.  'System' is currently unused.
                             (if (= ENOENT (system-error-errno args))
                                 base-info
                                 (apply throw args))))))
         (signature  (base64-encode-string
                      (canonical-sexp->string (signed-string info)))))
    (format #f "~aSignature: 1;~a;~a~%" info (gethostname) signature)))

(define (not-found request)
  "Render 404 response for REQUEST."
  (values (build-response #:code 404)
          (string-append "Resource not found: "
                         (uri-path (request-uri request)))))

(define (render-nix-cache-info)
  "Render server information."
  (values '((content-type . (text/plain)))
          (lambda (port)
            (for-each (match-lambda
                       ((key . value)
                        (format port "~a: ~a~%" key value)))
                      %nix-cache-info))))

(define (render-narinfo store request hash)
  "Render metadata for the store path corresponding to HASH."
  (let ((store-path (hash-part->path store hash)))
    (if (string-null? store-path)
        (not-found request)
        (values '((content-type . (application/x-nix-narinfo)))
                (cut display
                     (narinfo-string store store-path (force %private-key))
                     <>)))))

(define (render-nar store request store-item)
  "Render archive of the store path corresponding to STORE-ITEM."
  (let ((store-path (string-append %store-directory "/" store-item)))
    ;; The ISO-8859-1 charset *must* be used otherwise HTTP clients will
    ;; interpret the byte stream as UTF-8 and arbitrarily change invalid byte
    ;; sequences.
    (if (valid-path? store store-path)
        (values '((content-type . (application/x-nix-archive
                                   (charset . "ISO-8859-1"))))
                ;; XXX: We're not returning the actual contents, deferring
                ;; instead to 'http-write'.  This is a hack to work around
                ;; <http://bugs.gnu.org/21093>.
                store-path)
        (not-found request))))

(define extract-narinfo-hash
  (let ((regexp (make-regexp "^([a-df-np-sv-z0-9]{32}).narinfo$")))
    (lambda (str)
      "Return the hash within the narinfo resource string STR, or false if STR
is invalid."
      (and=> (regexp-exec regexp str)
             (cut match:substring <> 1)))))

(define (get-request? request)
  "Return #t if REQUEST uses the GET method."
  (eq? (request-method request) 'GET))

(define (request-path-components request)
  "Split the URI path of REQUEST into a list of component strings.  For
example: \"/foo/bar\" yields '(\"foo\" \"bar\")."
  (split-and-decode-uri-path (uri-path (request-uri request))))


;;;
;;; Server.
;;;

(define %http-write
  (@@ (web server http) http-write))

(define (sans-content-length response)
  "Return RESPONSE without its 'content-length' header."
  (set-field response (response-headers)
             (alist-delete 'content-length
                           (response-headers response)
                           eq?)))

(define-syntax-rule (swallow-EPIPE exp ...)
  "Swallow EPIPE errors raised by EXP..."
  (catch 'system-error
    (lambda ()
      exp ...)
    (lambda args
      (if (= EPIPE (system-error-errno args))
          (values)
          (apply throw args)))))

(define (http-write server client response body)
  "Write RESPONSE and BODY to CLIENT, possibly in a separate thread to avoid
blocking."
  (match (response-content-type response)
    (('application/x-nix-archive . _)
     ;; Sending the the whole archive can take time so do it in a separate
     ;; thread so that the main thread can keep working in the meantime.
     (call-with-new-thread
      (lambda ()
        (let* ((response (write-response (sans-content-length response)
                                         client))
               (port     (response-port response)))
          ;; XXX: Given our ugly workaround for <http://bugs.gnu.org/21093> in
          ;; 'render-nar', BODY here is just the file name of the store item.
          ;; We call 'write-file' from here because we know that's the only
          ;; way to avoid building the whole nar in memory, which could
          ;; quickly become a real problem.  As a bonus, we even do
          ;; sendfile(2) directly from the store files to the socket.
          (swallow-EPIPE
           (write-file (utf8->string body) port))
          (close-port port)
          (values)))))
    (_
     ;; Handle other responses sequentially.
     (%http-write server client response body))))

(define-server-impl concurrent-http-server
  ;; A variant of Guile's built-in HTTP server that offloads possibly long
  ;; responses to a different thread.
  (@@ (web server http) http-open)
  (@@ (web server http) http-read)
  http-write
  (@@ (web server http) http-close))

(define (make-request-handler store)
  (lambda (request body)
    (format #t "~a ~a~%"
            (request-method request)
            (uri-path (request-uri request)))
    (if (get-request? request) ; reject POST, PUT, etc.
        (match (request-path-components request)
          ;; /nix-cache-info
          (("nix-cache-info")
           (render-nix-cache-info))
          ;; /<hash>.narinfo
          (((= extract-narinfo-hash (? string? hash)))
           (render-narinfo store request hash))
          ;; /nar/<store-item>
          (("nar" store-item)
           (render-nar store request store-item))
          (_ (not-found request)))
        (not-found request))))

(define (run-publish-server socket store)
  (run-server (make-request-handler store)
              concurrent-http-server
              `(#:socket ,socket)))

(define (open-server-socket address)
  "Return a TCP socket bound to ADDRESS, a socket address."
  (let ((sock (socket (sockaddr:fam address) SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (bind sock address)
    sock))

(define (gather-user-privileges user)
  "Switch to the identity of USER, a user name."
  (catch 'misc-error
    (lambda ()
      (let ((user (getpw user)))
        (setgroups #())
        (setgid (passwd:gid user))
        (setuid (passwd:uid user))))
    (lambda (key proc message args . rest)
      (leave (_ "user '~a' not found: ~a~%")
             user (apply format #f message args)))))


;;;
;;; Entry point.
;;;

(define (guix-publish . args)
  (with-error-handling
    (let* ((opts    (args-fold* args %options
                                (lambda (opt name arg result)
                                  (leave (_ "~A: unrecognized option~%") name))
                                (lambda (arg result)
                                  (leave (_ "~A: extraneous argument~%") arg))
                                %default-options))
           (user    (assoc-ref opts 'user))
           (port    (assoc-ref opts 'port))
           (address (let ((addr (assoc-ref opts 'address)))
                      (make-socket-address (sockaddr:fam addr)
                                           (sockaddr:addr addr)
                                           port)))
           (socket  (open-server-socket address))
           (repl-port (assoc-ref opts 'repl)))
      ;; Read the key right away so that (1) we fail early on if we can't
      ;; access them, and (2) we can then drop privileges.
      (force %private-key)
      (force %public-key)

      (when user
        ;; Now that we've read the key material and opened the socket, we can
        ;; drop privileges.
        (gather-user-privileges user))

      (when (zero? (getuid))
        (warning (_ "server running as root; \
consider using the '--user' option!~%")))
      (format #t (_ "publishing ~a on ~a, port ~d~%")
              %store-directory
              (inet-ntop (sockaddr:fam address) (sockaddr:addr address))
              (sockaddr:port address))
      (when repl-port
        (repl:spawn-server (repl:make-tcp-server-socket #:port repl-port)))
      (with-store store
        (run-publish-server socket store)))))
