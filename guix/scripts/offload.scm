;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix scripts offload)
  #:use-module (ssh key)
  #:use-module (ssh auth)
  #:use-module (ssh session)
  #:use-module (ssh channel)
  #:use-module (ssh popen)
  #:use-module (ssh dist)
  #:use-module (ssh dist node)
  #:use-module (ssh version)
  #:use-module (guix config)
  #:use-module (guix records)
  #:use-module (guix ssh)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module ((guix serialization)
                #:select (nar-error? nar-error-file))
  #:use-module (guix nar)
  #:use-module (guix utils)
  #:use-module ((guix build syscalls)
                #:select (fcntl-flock set-thread-name))
  #:use-module ((guix build utils) #:select (which mkdir-p))
  #:use-module (guix ui)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (ice-9 binary-ports)
  #:export (build-machine
            build-requirements
            guix-offload))

;;; Commentary:
;;;
;;; Attempt to offload builds to the machines listed in
;;; /etc/guix/machines.scm, transferring missing dependencies over SSH, and
;;; retrieving the build output(s) over SSH upon success.
;;;
;;; This command should not be used directly; instead, it is called on-demand
;;; by the daemon, unless it was started with '--no-build-hook' or a client
;;; inhibited build hooks.
;;;
;;; Code:


(define-record-type* <build-machine>
  build-machine make-build-machine
  build-machine?
  (name            build-machine-name)            ; string
  (port            build-machine-port             ; number
                   (default 22))
  (system          build-machine-system)          ; string
  (user            build-machine-user)            ; string
  (private-key     build-machine-private-key      ; file name
                   (default (user-openssh-private-key)))
  (host-key        build-machine-host-key)        ; string
  (compression     build-machine-compression  ; string
                   (default "zlib@openssh.com,zlib"))
  (compression-level build-machine-compression-level ;integer
                     (default 3))
  (daemon-socket   build-machine-daemon-socket    ; string
                   (default "/var/guix/daemon-socket/socket"))
  (parallel-builds build-machine-parallel-builds  ; number
                   (default 1))
  (speed           build-machine-speed            ; inexact real
                   (default 1.0))
  (features        build-machine-features         ; list of strings
                   (default '())))

(define-record-type* <build-requirements>
  build-requirements make-build-requirements
  build-requirements?
  (system          build-requirements-system)     ; string
  (features        build-requirements-features    ; list of strings
                   (default '())))

(define %machine-file
  ;; File that lists machines available as build slaves.
  (string-append %config-directory "/machines.scm"))

(define (user-openssh-private-key)
  "Return the user's default SSH private key, or #f if it could not be
determined."
  (and=> (getenv "HOME")
         (cut string-append <> "/.ssh/id_rsa")))

(define %user-module
  ;; Module in which the machine description file is loaded.
  (let ((module (make-fresh-user-module)))
    (module-use! module (resolve-interface '(guix scripts offload)))
    module))

(define* (build-machines #:optional (file %machine-file))
  "Read the list of build machines from FILE and return it."
  (catch #t
    (lambda ()
      ;; Avoid ABI incompatibility with the <build-machine> record.
      ;; (set! %fresh-auto-compile #t)

      (save-module-excursion
       (lambda ()
         (set-current-module %user-module)
         (primitive-load file))))
    (lambda args
      (match args
        (('system-error . rest)
         (let ((err (system-error-errno args)))
           ;; Silently ignore missing file since this is a common case.
           (if (= ENOENT err)
               '()
               (leave (G_ "failed to open machine file '~a': ~a~%")
                      file (strerror err)))))
        (('syntax-error proc message properties form . rest)
         (let ((loc (source-properties->location properties)))
           (leave (G_ "~a: ~a~%")
                  (location->string loc) message)))
        (x
         (leave (G_ "failed to load machine file '~a': ~s~%")
                file args))))))

(define (host-key->type+key host-key)
  "Destructure HOST-KEY, an OpenSSH host key string, and return two values:
its key type as a symbol, and the actual base64-encoded string."
  (define (type->symbol type)
    (and (string-prefix? "ssh-" type)
         (string->symbol (string-drop type 4))))

  (match (string-tokenize host-key)
    ((type key x)
     (values (type->symbol type) key))
    ((type key)
     (values (type->symbol type) key))))

(define (private-key-from-file* file)
  "Like 'private-key-from-file', but raise an error that 'with-error-handling'
can interpret meaningfully."
  (catch 'guile-ssh-error
    (lambda ()
      (private-key-from-file file))
    (lambda (key proc str . rest)
      (raise (condition
              (&message (message (format #f (G_ "failed to load SSH \
private key from '~a': ~a")
                                         file str))))))))

(define (open-ssh-session machine)
  "Open an SSH session for MACHINE and return it.  Throw an error on failure."
  (let ((private (private-key-from-file* (build-machine-private-key machine)))
        (public  (public-key-from-file
                  (string-append (build-machine-private-key machine)
                                 ".pub")))
        (session (make-session #:user (build-machine-user machine)
                               #:host (build-machine-name machine)
                               #:port (build-machine-port machine)
                               #:timeout 10       ;seconds
                               ;; #:log-verbosity 'protocol
                               #:identity (build-machine-private-key machine)

                               ;; By default libssh reads ~/.ssh/known_hosts
                               ;; and uses that to adjust its choice of cipher
                               ;; suites, which changes the type of host key
                               ;; that the server sends (RSA vs. Ed25519,
                               ;; etc.).  Opt for something reproducible and
                               ;; stateless instead.
                               #:knownhosts "/dev/null"

                               ;; We need lightweight compression when
                               ;; exchanging full archives.
                               #:compression
                               (build-machine-compression machine)
                               #:compression-level
                               (build-machine-compression-level machine))))
    (match (connect! session)
      ('ok
       ;; Authenticate the server.  XXX: Guile-SSH 0.10.1 doesn't know about
       ;; ed25519 keys and 'get-key-type' returns #f in that case.
       (let-values (((server)   (get-server-public-key session))
                    ((type key) (host-key->type+key
                                 (build-machine-host-key machine))))
         (unless (and (or (not (get-key-type server))
                          (eq? (get-key-type server) type))
                      (string=? (public-key->string server) key))
           ;; Key mismatch: something's wrong.  XXX: It could be that the server
           ;; provided its Ed25519 key when we where expecting its RSA key.
           (leave (G_ "server at '~a' returned host key '~a' of type '~a' \
instead of '~a' of type '~a'~%")
                  (build-machine-name machine)
                  (public-key->string server) (get-key-type server)
                  key type)))

       (let ((auth (userauth-public-key! session private)))
         (unless (eq? 'success auth)
           (disconnect! session)
           (leave (G_ "SSH public key authentication failed for '~a': ~a~%")
                  (build-machine-name machine) (get-error session))))

       session)
      (x
       ;; Connection failed or timeout expired.
       (leave (G_ "failed to connect to '~a': ~a~%")
              (build-machine-name machine) (get-error session))))))


;;;
;;; Synchronization.
;;;

(define (lock-file file)
  "Wait and acquire an exclusive lock on FILE.  Return an open port."
  (mkdir-p (dirname file))
  (let ((port (open-file file "w0")))
    (fcntl-flock port 'write-lock)
    port))

(define (unlock-file lock)
  "Unlock LOCK."
  (fcntl-flock lock 'unlock)
  (close-port lock)
  #t)

(define-syntax-rule (with-file-lock file exp ...)
  "Wait to acquire a lock on FILE and evaluate EXP in that context."
  (let ((port (lock-file file)))
    (dynamic-wind
      (lambda ()
        #t)
      (lambda ()
        exp ...)
      (lambda ()
        (unlock-file port)))))

(define-syntax-rule (with-machine-lock machine hint exp ...)
  "Wait to acquire MACHINE's exclusive lock for HINT, and evaluate EXP in that
context."
  (with-file-lock (machine-lock-file machine hint)
    exp ...))


(define (machine-slot-file machine slot)
  "Return the file name of MACHINE's file for SLOT."
  ;; For each machine we have a bunch of files representing each build slot.
  ;; When choosing a build machine, we attempt to get an exclusive lock on one
  ;; of these; if we fail, that means all the build slots are already taken.
  ;; Inspired by Nix's build-remote.pl.
  (string-append  (string-append %state-directory "/offload/"
                                 (build-machine-name machine)
                                 "/" (number->string slot))))

(define (acquire-build-slot machine)
  "Attempt to acquire a build slot on MACHINE.  Return the port representing
the slot, or #f if none is available.

This mechanism allows us to set a hard limit on the number of simultaneous
connections allowed to MACHINE."
  (mkdir-p (dirname (machine-slot-file machine 0)))
  (with-machine-lock machine 'slots
    (any (lambda (slot)
           (let ((port (open-file (machine-slot-file machine slot)
                                  "w0")))
             (catch 'flock-error
               (lambda ()
                 (fcntl-flock port 'write-lock #:wait? #f)
                 ;; Got it!
                 (format (current-error-port)
                         "process ~a acquired build slot '~a'~%"
                         (getpid) (port-filename port))
                 port)
               (lambda args
                 ;; PORT is already locked by another process.
                 (close-port port)
                 #f))))
         (iota (build-machine-parallel-builds machine)))))

(define (release-build-slot slot)
  "Release SLOT, a build slot as returned as by 'acquire-build-slot'."
  (close-port slot))


;;;
;;; Offloading.
;;;

(define (build-log-port)
  "Return the default port where build logs should be sent.  The default is
file descriptor 4, which is open by the daemon before running the offload
hook."
  (let ((port (fdopen 4 "w0")))
    ;; Make sure file descriptor 4 isn't closed when PORT is GC'd.
    (set-port-revealed! port 1)
    port))

(define* (transfer-and-offload drv machine
                               #:key
                               (inputs '())
                               (outputs '())
                               (max-silent-time 3600)
                               build-timeout
                               print-build-trace?)
  "Offload DRV to MACHINE.  Prior to the actual offloading, transfer all of
INPUTS to MACHINE; if building DRV succeeds, retrieve all of OUTPUTS from
MACHINE."
  (define session
    (open-ssh-session machine))

  (define store
    (connect-to-remote-daemon session
                              (build-machine-daemon-socket machine)))

  (set-build-options store
                     #:print-build-trace print-build-trace?
                     #:max-silent-time max-silent-time
                     #:timeout build-timeout)

  ;; Protect DRV from garbage collection.
  (add-temp-root store (derivation-file-name drv))

  (with-store local
    (send-files local (cons (derivation-file-name drv) inputs) store
                #:log-port (current-output-port)))
  (format (current-error-port) "offloading '~a' to '~a'...~%"
          (derivation-file-name drv) (build-machine-name machine))
  (format (current-error-port) "@ build-remote ~a ~a~%"
          (derivation-file-name drv) (build-machine-name machine))

  (guard (c ((nix-protocol-error? c)
             (format (current-error-port)
                     (G_ "derivation '~a' offloaded to '~a' failed: ~a~%")
                     (derivation-file-name drv)
                     (build-machine-name machine)
                     (nix-protocol-error-message c))
             ;; Use exit code 100 for a permanent build failure.  The daemon
             ;; interprets other non-zero codes as transient build failures.
             (primitive-exit 100)))
    (parameterize ((current-build-output-port (build-log-port)))
      (build-derivations store (list drv))))

  (retrieve-files* outputs store)
  (format (current-error-port) "done with offloaded '~a'~%"
          (derivation-file-name drv)))

(define (retrieve-files* files remote)
  "Retrieve FILES from REMOTE and import them using 'restore-file-set'."
  (let-values (((port count)
                (file-retrieval-port files remote)))
    (format #t (N_ "retrieving ~a store item from '~a'...~%"
                   "retrieving ~a store items from '~a'...~%" count)
            count (remote-store-host remote))

    ;; We cannot use the 'import-paths' RPC here because we already
    ;; hold the locks for FILES.
    (let ((result (restore-file-set port
                                    #:log-port (current-error-port)
                                    #:lock? #f)))
      (close-port port)
      result)))


;;;
;;; Scheduling.
;;;

(define (machine-matches? machine requirements)
  "Return #t if MACHINE matches REQUIREMENTS."
  (and (string=? (build-requirements-system requirements)
                 (build-machine-system machine))
       (lset<= string=?
               (build-requirements-features requirements)
               (build-machine-features machine))))

(define (machine-load machine)
  "Return the load of MACHINE, divided by the number of parallel builds
allowed on MACHINE.  Return +∞ if MACHINE is unreachable."
  ;; Note: This procedure is costly since it creates a new SSH session.
  (match (false-if-exception (open-ssh-session machine))
    ((? session? session)
     (let* ((pipe (open-remote-pipe* session OPEN_READ
                                     "cat" "/proc/loadavg"))
            (line (read-line pipe)))
       (close-port pipe)
       (disconnect! session)

       (if (eof-object? line)
           +inf.0 ;MACHINE does not respond, so assume it is infinitely loaded
           (match (string-tokenize line)
             ((one five fifteen . x)
              (let* ((raw        (string->number five))
                     (jobs       (build-machine-parallel-builds machine))
                     (normalized (/ raw jobs)))
                (format (current-error-port) "load on machine '~a' is ~s\
 (normalized: ~s)~%"
                        (build-machine-name machine) raw normalized)
                normalized))
             (x
              +inf.0)))))        ;something's fishy about MACHINE, so avoid it
    (x
     +inf.0)))                      ;failed to connect to MACHINE, so avoid it

(define (machine-lock-file machine hint)
  "Return the name of MACHINE's lock file for HINT."
  (string-append %state-directory "/offload/"
                 (build-machine-name machine)
                 "." (symbol->string hint) ".lock"))

(define (machine-choice-lock-file)
  "Return the name of the file used as a lock when choosing a build machine."
  (string-append %state-directory "/offload/machine-choice.lock"))

(define (random-seed)
  (logxor (getpid) (car (gettimeofday))))

(define shuffle
  (let ((state (seed->random-state (random-seed))))
    (lambda (lst)
      "Return LST shuffled (using the Fisher-Yates algorithm.)"
      (define vec (list->vector lst))
      (let loop ((result '())
                 (i (vector-length vec)))
        (if (zero? i)
            result
            (let* ((j (random i state))
                   (val (vector-ref vec j)))
              (vector-set! vec j (vector-ref vec (- i 1)))
              (loop (cons val result) (- i 1))))))))

(define (choose-build-machine machines)
  "Return two values: the best machine among MACHINES and its build
slot (which must later be released with 'release-build-slot'), or #f and #f."

  ;; Proceed like this:
  ;;   1. Acquire the global machine-choice lock.
  ;;   2. For all MACHINES, attempt to acquire a build slot, and filter out
  ;;      those machines for which we failed.
  ;;   3. Choose the best machine among those that are left.
  ;;   4. Release the previously-acquired build slots of the other machines.
  ;;   5. Release the global machine-choice lock.

  (with-file-lock (machine-choice-lock-file)
    (define machines+slots
      (filter-map (lambda (machine)
                    (let ((slot (acquire-build-slot machine)))
                      (and slot (list machine slot))))
                  (shuffle machines)))

    (define (undecorate pred)
      (lambda (a b)
        (match a
          ((machine1 slot1)
           (match b
             ((machine2 slot2)
              (pred machine1 machine2)))))))

    (define (machine-faster? m1 m2)
      ;; Return #t if M1 is faster than M2.
      (> (build-machine-speed m1)
         (build-machine-speed m2)))

    (let loop ((machines+slots
                (sort machines+slots (undecorate machine-faster?))))
      (match machines+slots
        (((best slot) others ...)
         ;; Return the best machine unless it's already overloaded.
         ;; Note: We call 'machine-load' only as a last resort because it is
         ;; too costly to call it once for every machine.
         (if (< (machine-load best) 2.)
             (match others
               (((machines slots) ...)
                ;; Release slots from the uninteresting machines.
                (for-each release-build-slot slots)

                ;; The caller must keep SLOT to protect it from GC and to
                ;; eventually release it.
                (values best slot)))
             (begin
               ;; BEST is overloaded, so try the next one.
               (release-build-slot slot)
               (loop others))))
        (()
         (values #f #f))))))

(define* (process-request wants-local? system drv features
                          #:key
                          print-build-trace? (max-silent-time 3600)
                          build-timeout)
  "Process a request to build DRV."
  (let* ((local?     (and wants-local? (string=? system (%current-system))))
         (reqs       (build-requirements
                      (system system)
                      (features features)))
         (candidates (filter (cut machine-matches? <> reqs)
                             (build-machines))))
    (match candidates
      (()
       ;; We'll never be able to match REQS.
       (display "# decline\n"))
      ((x ...)
       (let-values (((machine slot)
                     (choose-build-machine candidates)))
         (if machine
             (dynamic-wind
               (const #f)
               (lambda ()
                 ;; Offload DRV to MACHINE.
                 (display "# accept\n")
                 (let ((inputs  (string-tokenize (read-line)))
                       (outputs (string-tokenize (read-line))))
                   (transfer-and-offload drv machine
                                         #:inputs inputs
                                         #:outputs outputs
                                         #:max-silent-time max-silent-time
                                         #:build-timeout build-timeout
                                         #:print-build-trace?
                                         print-build-trace?)))
               (lambda ()
                 (release-build-slot slot)))

             ;; Not now, all the machines are busy.
             (display "# postpone\n")))))))


;;;
;;; Installation tests.
;;;

(define (assert-node-repl node name)
  "Bail out if NODE is not running Guile."
  (match (node-guile-version node)
    (#f
     (leave (G_ "Guile could not be started on '~a'~%")
            name))
    ((? string? version)
     ;; Note: The version string already contains the word "Guile".
     (info (G_ "'~a' is running ~a~%")
           name (node-guile-version node)))))

(define (assert-node-has-guix node name)
  "Bail out if NODE lacks the (guix) module, or if its daemon is not running."
  (match (node-eval node
                    '(begin
                       (use-modules (guix))
                       (with-store store
                         (add-text-to-store store "test"
                                            "Hello, build machine!"))))
    ((? string? str)
     (info (G_ "Guix is usable on '~a' (test returned ~s)~%")
           name str))
    (x
     (leave (G_ "failed to use Guix module on '~a' (test returned ~s)~%")
            name x))))

(define %random-state
  (delay
    (seed->random-state (logxor (getpid) (car (gettimeofday))))))

(define* (nonce #:optional (name (gethostname)))
  (string-append name "-"
                 (number->string (random 1000000 (force %random-state)))))

(define (assert-node-can-import node name daemon-socket)
  "Bail out if NODE refuses to import our archives."
  (let ((session (node-session node)))
    (with-store store
      (let* ((item   (add-text-to-store store "export-test" (nonce)))
             (remote (connect-to-remote-daemon session daemon-socket)))
        (with-store local
          (send-files local (list item) remote))

        (if (valid-path? remote item)
            (info (G_ "'~a' successfully imported '~a'~%")
                  name item)
            (leave (G_ "'~a' was not properly imported on '~a'~%")
                   item name))))))

(define (assert-node-can-export node name daemon-socket)
  "Bail out if we cannot import signed archives from NODE."
  (let* ((session (node-session node))
         (remote  (connect-to-remote-daemon session daemon-socket))
         (item    (add-text-to-store remote "import-test" (nonce name))))
    (with-store store
      (if (and (retrieve-files store (list item) remote)
               (valid-path? store item))
          (info (G_ "successfully imported '~a' from '~a'~%")
                item name)
          (leave (G_ "failed to import '~a' from '~a'~%")
                 item name)))))

(define (check-machine-availability machine-file pred)
  "Check that each machine matching PRED in MACHINE-FILE is usable as a build
machine."
  (define (build-machine=? m1 m2)
    (and (string=? (build-machine-name m1) (build-machine-name m2))
         (= (build-machine-port m1) (build-machine-port m2))))

  ;; A given build machine may appear several times (e.g., once for
  ;; "x86_64-linux" and a second time for "i686-linux"); test them only once.
  (let ((machines (filter pred
                          (delete-duplicates (build-machines machine-file)
                                             build-machine=?))))
    (info (G_ "testing ~a build machines defined in '~a'...~%")
          (length machines) machine-file)
    (let* ((names    (map build-machine-name machines))
           (sockets  (map build-machine-daemon-socket machines))
           (sessions (map open-ssh-session machines))
           (nodes    (map make-node sessions)))
      (for-each assert-node-repl nodes names)
      (for-each assert-node-has-guix nodes names)
      (for-each assert-node-can-import nodes names sockets)
      (for-each assert-node-can-export nodes names sockets))))

(define (check-machine-status machine-file pred)
  "Print the load of each machine matching PRED in MACHINE-FILE."
  (define (build-machine=? m1 m2)
    (and (string=? (build-machine-name m1) (build-machine-name m2))
         (= (build-machine-port m1) (build-machine-port m2))))

  ;; A given build machine may appear several times (e.g., once for
  ;; "x86_64-linux" and a second time for "i686-linux"); test them only once.
  (let ((machines (filter pred
                          (delete-duplicates (build-machines machine-file)
                                             build-machine=?))))
    (info (G_ "getting status of ~a build machines defined in '~a'...~%")
          (length machines) machine-file)
    (for-each (lambda (machine)
                (let* ((node (make-node (open-ssh-session machine)))
                       (uts (node-eval node '(uname))))
                  (format #t "~a~%  kernel: ~a ~a~%  architecture: ~a~%\
  host name: ~a~%  normalized load: ~a~%"
                          (build-machine-name machine)
                          (utsname:sysname uts) (utsname:release uts)
                          (utsname:machine uts)
                          (utsname:nodename uts)
                          (parameterize ((current-error-port (%make-void-port "rw+")))
                                        (machine-load machine)))))
              machines)))


;;;
;;; Entry point.
;;;

(define (guix-offload . args)
  (define request-line-rx
    ;; The request format.  See 'tryBuildHook' method in build.cc.
    (make-regexp "([01]) ([a-z0-9_-]+) (/[[:graph:]]+.drv) ([[:graph:]]*)"))

  (define not-coma
    (char-set-complement (char-set #\,)))

  ;; Make sure $HOME really corresponds to the current user.  This is
  ;; necessary since lsh uses that to determine the location of the yarrow
  ;; seed file, and fails if it's owned by someone else.
  (and=> (passwd:dir (getpw (getuid)))
         (cut setenv "HOME" <>))

  ;; We rely on protocol-level compression from libssh to optimize large data
  ;; transfers.  Warn if it's missing.
  (unless (zlib-support?)
    (warning (G_ "Guile-SSH lacks zlib support"))
    (warning (G_ "data transfers will *not* be compressed!")))

  (match args
    ((system max-silent-time print-build-trace? build-timeout)
     (let ((max-silent-time    (string->number max-silent-time))
           (build-timeout      (string->number build-timeout))
           (print-build-trace? (string=? print-build-trace? "1")))
       (set-thread-name "guix offload")
       (parameterize ((%current-system system))
         (let loop ((line (read-line)))
           (unless (eof-object? line)
             (cond ((regexp-exec request-line-rx line)
                    =>
                    (lambda (match)
                      (with-error-handling
                       (process-request (equal? (match:substring match 1) "1")
                                        (match:substring match 2) ; system
                                        (read-derivation-from-file
                                         (match:substring match 3))
                                        (string-tokenize
                                         (match:substring match 4) not-coma)
                                        #:print-build-trace? print-build-trace?
                                        #:max-silent-time max-silent-time
                                        #:build-timeout build-timeout))))
                   (else
                    (leave (G_ "invalid request line: ~s~%") line)))
             (loop (read-line)))))))
    (("test" rest ...)
     (with-error-handling
       (let-values (((file pred)
                     (match rest
                       ((file regexp)
                        (values file
                                (compose (cut string-match regexp <>)
                                         build-machine-name)))
                       ((file) (values file (const #t)))
                       (()     (values %machine-file (const #t)))
                       (x      (leave (G_ "wrong number of arguments~%"))))))
         (check-machine-availability (or file %machine-file) pred))))
    (("status" rest ...)
     (with-error-handling
       (let-values (((file pred)
                     (match rest
                       ((file regexp)
                        (values file
                                (compose (cut string-match regexp <>)
                                         build-machine-name)))
                       ((file) (values file (const #t)))
                       (()     (values %machine-file (const #t)))
                       (x      (leave (G_ "wrong number of arguments~%"))))))
         (check-machine-status (or file %machine-file) pred))))
    (("--version")
     (show-version-and-exit "guix offload"))
    (("--help")
     (format #t (G_ "Usage: guix offload SYSTEM PRINT-BUILD-TRACE
Process build offload requests written on the standard input, possibly
offloading builds to the machines listed in '~a'.~%")
             %machine-file)
     (display (G_ "
This tool is meant to be used internally by 'guix-daemon'.\n"))
     (show-bug-report-information))
    (x
     (leave (G_ "invalid arguments: ~{~s ~}~%") x))))

;;; Local Variables:
;;; eval: (put 'with-machine-lock 'scheme-indent-function 2)
;;; eval: (put 'with-file-lock 'scheme-indent-function 1)
;;; eval: (put 'with-error-to-port 'scheme-indent-function 1)
;;; End:

;;; offload.scm ends here
