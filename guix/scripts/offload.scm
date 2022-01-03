;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
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
  #:autoload   (ssh key) (private-key-from-file
                          public-key-from-file)
  #:autoload   (ssh auth) (userauth-public-key!)
  #:autoload   (ssh session) (make-session
                              connect! get-error
                              disconnect! session-set!)
  #:autoload   (ssh version) (zlib-support?)
  #:use-module (guix config)
  #:use-module (guix records)
  #:autoload   (guix ssh) (authenticate-server*
                           connect-to-remote-daemon
                           send-files retrieve-files retrieve-files*
                           remote-inferior report-guile-error)
  #:use-module (guix store)
  #:autoload   (guix inferior) (inferior-eval close-inferior inferior?)
  #:autoload   (guix derivations) (read-derivation-from-file
                                   derivation-file-name
                                   build-derivations)
  #:autoload   (guix serialization) (nar-error? nar-error-file)
  #:autoload   (guix nar) (restore-file-set)
  #:use-module ((guix utils) #:select (%current-system))
  #:use-module ((guix build syscalls)
                #:select (fcntl-flock set-thread-name))
  #:use-module ((guix build utils) #:select (which mkdir-p))
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix diagnostics)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:export (build-machine
            build-machine?
            build-machine-name
            build-machine-port
            build-machine-systems
            build-machine-user
            build-machine-private-key
            build-machine-host-key
            build-machine-compression
            build-machine-daemon-socket
            build-machine-overload-threshold
            build-machine-systems
            build-machine-features
            build-machine-location

            build-requirements
            build-requirements?

            guix-offload))

;;; Commentary:
;;;
;;; Attempt to offload builds to the machines listed in
;;; /etc/guix/machines.scm, transferring missing dependencies over SSH, and
;;; retrieving the build output(s) over SSH upon success.
;;;
;;; This command should not be used directly; instead, it is called on-demand
;;; by the daemon, unless it was started with '--no-offload' or a client
;;; inhibited build hooks.
;;;
;;; Code:

(define-record-type* <build-machine>
  build-machine make-build-machine
  build-machine?
  (name            build-machine-name)            ; string
  (port            build-machine-port             ; number
                   (default 22))
  (systems         %build-machine-systems         ; list of strings
                   (default #f))                  ; drop default after system is removed
  (system          %build-machine-system          ; deprecated
                   (default #f))
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
  ;; A #f value tells the offload scheduler to disregard the load of the build
  ;; machine when selecting the best offload machine.
  (overload-threshold build-machine-overload-threshold ; inexact real between
                      (default 0.6))                   ; 0.0 and 1.0 | #f
  (parallel-builds build-machine-parallel-builds  ; number
                   (default 1))
  (speed           build-machine-speed            ; inexact real
                   (default 1.0))
  (features        build-machine-features         ; list of strings
                   (default '()))
  (location        build-machine-location
                   (default (and=> (current-source-location)
                                   source-properties->location))
                   (innate)))

;;; Deprecated.
(define (build-machine-system machine)
  (warning
    (build-machine-location machine)
    (G_ "The 'system' field is deprecated, \
please use 'systems' instead.~%"))
  (%build-machine-system machine))

;;; TODO: Remove after the deprecated 'system' field is removed.
(define (build-machine-systems machine)
  (or (%build-machine-systems machine)
      (list (build-machine-system machine))
      (leave (G_ "The build-machine object lacks a value for its 'systems'
field."))))

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
         (match (primitive-load file)
           (((? build-machine? machines) ...)
            machines)
           (_
            ;; Instead of crashing, assume the empty list.
            (warning (G_ "'~a' did not return a list of build machines; \
ignoring it~%")
                     file)
            '())))))
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

(define (private-key-from-file* file)
  "Like 'private-key-from-file', but raise an error that 'with-error-handling'
can interpret meaningfully."
  (catch 'guile-ssh-error
    (lambda ()
      (private-key-from-file file))
    (lambda (key proc str . rest)
      (raise (formatted-message (G_ "failed to load SSH \
private key from '~a': ~a")
                                file str)))))

(define* (open-ssh-session machine #:optional max-silent-time)
  "Open an SSH session for MACHINE and return it.  Throw an error on failure.
When MAX-SILENT-TIME is true, it must be a positive integer denoting the
number of seconds after which the connection times out."
  (let ((private (private-key-from-file* (build-machine-private-key machine)))
        (public  (public-key-from-file
                  (string-append (build-machine-private-key machine)
                                 ".pub")))
        (session (make-session #:user (build-machine-user machine)
                               #:host (build-machine-name machine)
                               #:port (build-machine-port machine)
                               #:timeout 10       ;initial timeout (seconds)
                               ;; #:log-verbosity 'protocol
                               #:identity (build-machine-private-key machine)

                               ;; By default libssh reads ~/.ssh/known_hosts
                               ;; and uses that to adjust its choice of cipher
                               ;; suites, which changes the type of host key
                               ;; that the server sends (RSA vs. Ed25519,
                               ;; etc.).  Opt for something reproducible and
                               ;; stateless instead.
                               #:knownhosts "/dev/null"

                               ;; Likewise for ~/.ssh/config.
                               #:config "/dev/null"

                               ;; We need lightweight compression when
                               ;; exchanging full archives.
                               #:compression
                               (build-machine-compression machine)
                               #:compression-level
                               (build-machine-compression-level machine))))
    (match (connect! session)
      ('ok
       ;; Make sure the server's key is what we expect.
       (authenticate-server* session (build-machine-host-key machine))

       (let ((auth (userauth-public-key! session private)))
         (unless (eq? 'success auth)
           (disconnect! session)
           (leave (G_ "SSH public key authentication failed for '~a': ~a~%")
                  (build-machine-name machine) (get-error session))))

       (when max-silent-time
         ;; From then on use MAX-SILENT-TIME as the absolute timeout when
         ;; reading from or write to a channel for this session.
         (session-set! session 'timeout max-silent-time))

       session)
      (x
       ;; Connection failed or timeout expired.
       (leave (G_ "failed to connect to '~a': ~a~%")
              (build-machine-name machine) (get-error session))))))


;;;
;;; Synchronization.
;;;

(define (machine-slot-file machine slot)
  "Return the file name of MACHINE's file for SLOT."
  ;; For each machine we have a bunch of files representing each build slot.
  ;; When choosing a build machine, we attempt to get an exclusive lock on one
  ;; of these; if we fail, that means all the build slots are already taken.
  ;; Inspired by Nix's build-remote.pl.
  (string-append  (string-append %state-directory "/offload/"
                                 (build-machine-name machine) ":"
                                 (number->string (build-machine-port machine))
                                 "/" (number->string slot))))

(define (acquire-build-slot machine)
  "Attempt to acquire a build slot on MACHINE.  Return the port representing
the slot, or #f if none is available.

This mechanism allows us to set a hard limit on the number of simultaneous
connections allowed to MACHINE."
  (mkdir-p (dirname (machine-slot-file machine 0)))

  ;; When several 'guix offload' processes run in parallel, there's a race
  ;; among them, but since they try the slots in the same order, we're fine.
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
       (iota (build-machine-parallel-builds machine))))

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

(define (node-guile-version node)
  (inferior-eval '(version) node))

(define (node-free-disk-space node)
  "Return the free disk space, in bytes, in NODE's store."
  (inferior-eval `(begin
                    (use-modules (guix build syscalls))
                    (free-disk-space ,(%store-prefix)))
                 node))

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
    (open-ssh-session machine max-silent-time))

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

  (guard (c ((store-protocol-error? c)
             (format (current-error-port)
                     (G_ "derivation '~a' offloaded to '~a' failed: ~a~%")
                     (derivation-file-name drv)
                     (build-machine-name machine)
                     (store-protocol-error-message c))
             (let* ((inferior (false-if-exception (remote-inferior session)))
                    (space (false-if-exception
                            (node-free-disk-space inferior))))

               (when inferior
                 (close-inferior inferior))

               ;; Use exit code 100 for a permanent build failure.  The daemon
               ;; interprets other non-zero codes as transient build failures.
               (if (and space (< space (* 10 (expt 2 20))))
                   (begin
                     (format (current-error-port)
                             (G_ "build failure may have been caused by lack \
of free disk space on '~a'~%")
                             (build-machine-name machine))
                     (primitive-exit 1))
                   (primitive-exit 100)))))
    (parameterize ((current-build-output-port (build-log-port)))
      (build-derivations store (list drv))))

  (retrieve-files* outputs store

                   ;; We cannot use the 'import-paths' RPC here because we
                   ;; already hold the locks for FILES.
                   #:import
                   (lambda (port)
                     (restore-file-set port
                                       #:log-port (current-error-port)
                                       #:lock? #f)))

  (close-connection store)
  (disconnect! session)
  (format (current-error-port) "done with offloaded '~a'~%"
          (derivation-file-name drv)))


;;;
;;; Scheduling.
;;;

(define (machine-matches? machine requirements)
  "Return #t if MACHINE matches REQUIREMENTS."
  (and (member (build-requirements-system requirements)
               (build-machine-systems machine))
       (lset<= string=?
               (build-requirements-features requirements)
               (build-machine-features machine))))

(define %minimum-disk-space
  ;; Minimum disk space required on the build machine for a build to be
  ;; offloaded.  This keeps us from offloading to machines that are bound to
  ;; run out of disk space.
  (* 100 (expt 2 20)))                            ;100 MiB

(define (node-load node)
  "Return the load on NODE, a normalized value between 0.0 and 1.0.  The value
is derived from /proc/loadavg and normalized according to the number of
logical cores available, to give a rough estimation of CPU usage.  Return
1.0 (fully loaded) if NODE is misbehaving."
  (let ((line (inferior-eval '(begin
                                (use-modules (ice-9 rdelim))
                                (call-with-input-file "/proc/loadavg"
                                  read-string))
                             node))
        (ncores (inferior-eval '(begin
                                  (use-modules (ice-9 threads))
                                  (current-processor-count))
                               node)))
    (if (or (eof-object? line) (eof-object? ncores))
        1.0    ;MACHINE does not respond, so assume it is fully loaded
        (match (string-tokenize line)
          ((one five fifteen . x)
           (let ((load (/ (string->number one) ncores)))
             (if (> load 1.0)
                 1.0
                 load)))
          (x
           1.0)))))

(define (report-load machine load)
  (format (current-error-port)
          "normalized load on machine '~a' is ~,2f~%"
          (build-machine-name machine) load))

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
  ;;   1. For all MACHINES, attempt to acquire a build slot, and filter out
  ;;      those machines for which we failed.
  ;;   2. Choose the best machine among those that are left.
  ;;   3. Release the previously-acquired build slots of the other machines.

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
       ;; Note: We call 'node-load' only as a last resort because it is
       ;; too costly to call it once for every machine.
       (let* ((session (false-if-exception (open-ssh-session best
                                                             %short-timeout)))
              (node    (and session (remote-inferior session)))
              (load    (and node (node-load node)))
              (threshold (build-machine-overload-threshold best))
              (space   (and node (node-free-disk-space node))))
         (when load (report-load best load))
         (when node (close-inferior node))
         (when session (disconnect! session))
         (if (and node
                  (or (not threshold) (< load threshold))
                  (>= space %minimum-disk-space))
             (match others
               (((machines slots) ...)
                ;; Release slots from the uninteresting machines.
                (for-each release-build-slot slots)

                ;; The caller must keep SLOT to protect it from GC and to
                ;; eventually release it.
                (values best slot)))
             (begin
               ;; BEST is unsuitable, so try the next one.
               (when (and space (< space %minimum-disk-space))
                 (format (current-error-port)
                         "skipping machine '~a' because it is low \
on disk space (~,2f MiB free)~%"
                         (build-machine-name best)
                         (/ space (expt 2 20) 1.)))
               (release-build-slot slot)
               (loop others)))))
      (()
       (values #f #f)))))

(define (call-with-timeout timeout drv thunk)
  "Call THUNK and leave after TIMEOUT seconds.  If TIMEOUT is #f, simply call
THUNK.  Use DRV as an indication of what we were building when the timeout
expired."
  (if (number? timeout)
      (dynamic-wind
        (lambda ()
          (sigaction SIGALRM
            (lambda _
              ;; The exit code here will be 1, which guix-daemon will
              ;; interpret as a transient failure.
              (leave (G_ "timeout expired while offloading '~a'~%")
                     (derivation-file-name drv))))
          (alarm timeout))
        thunk
        (lambda ()
          (alarm 0)))
      (thunk)))

(define-syntax-rule (with-timeout timeout drv exp ...)
  "Evaluate EXP... and leave after TIMEOUT seconds if EXP hasn't completed.
If TIMEOUT is #f, simply evaluate EXP..."
  (call-with-timeout timeout drv (lambda () exp ...)))

(define (check-ssh-zlib-support)
  "Warn once if libssh lacks zlib support."
  ;; We rely on protocol-level compression from libssh to optimize large data
  ;; transfers.  Warn if it's missing.
  (unless (zlib-support?)
    (warning (G_ "Guile-SSH lacks zlib support"))
    (warning (G_ "data transfers will *not* be compressed!")))
  (set! check-ssh-zlib-support (const #t)))

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
                 (check-ssh-zlib-support)
                 (let ((drv     (read-derivation-from-file drv))
                       (inputs  (string-tokenize (read-line)))
                       (outputs (string-tokenize (read-line))))
                   ;; Even if BUILD-TIMEOUT is honored by MACHINE, there can
                   ;; be issues with the connection or deadlocks that could
                   ;; lead the 'guix offload' process to remain stuck forever.
                   ;; To avoid that, install a timeout here as well.
                   (with-timeout build-timeout drv
                     (transfer-and-offload drv machine
                                           #:inputs inputs
                                           #:outputs outputs
                                           #:max-silent-time max-silent-time
                                           #:build-timeout build-timeout
                                           #:print-build-trace?
                                           print-build-trace?))))
               (lambda ()
                 (release-build-slot slot)))

             ;; Not now, all the machines are busy.
             (display "# postpone\n")))))))


;;;
;;; Installation tests.
;;;

(define %short-timeout
  ;; Timeout in seconds used on SSH connections where reads and writes
  ;; shouldn't take long.
  15)

(define (assert-node-repl node name)
  "Bail out if NODE is not running Guile."
  (match (node-guile-version node)
    (#f
     (report-guile-error name))
    ((? string? version)
     (info (G_ "'~a' is running GNU Guile ~a~%")
           name (node-guile-version node)))))

(define (assert-node-has-guix node name)
  "Bail out if NODE if #f or if we fail to use the (guix) module, or if its
daemon is not running."
  (unless (inferior? node)
    (leave (G_ "failed to run 'guix repl' on '~a'~%") name))

  (match (inferior-eval '(begin
                           (use-modules (guix))
                           (and add-text-to-store 'alright))
                        node)
    ('alright #t)
    (_ (leave (G_ "(guix) module not usable on remote host '~a'")
              name)))

  (match (inferior-eval '(begin
                           (use-modules (guix))
                           (with-store store
                             (add-text-to-store store "test"
                                                "Hello, build machine!")))
                        node)
    ((? string? str)
     (info (G_ "Guix is usable on '~a' (test returned ~s)~%")
           name str))
    (x
     (leave (G_ "failed to talk to guix-daemon on '~a' (test returned ~s)~%")
            name x))))

(define %random-state
  (delay
    (seed->random-state (logxor (getpid) (car (gettimeofday))))))

(define* (nonce #:optional (name (gethostname)))
  (string-append name "-"
                 (number->string (random 1000000 (force %random-state)))))

(define (assert-node-can-import session node name daemon-socket)
  "Bail out if NODE refuses to import our archives."
  (with-store store
    (let* ((item   (add-text-to-store store "export-test" (nonce)))
           (remote (connect-to-remote-daemon session daemon-socket)))
      (with-store local
        (send-files local (list item) remote))

      (if (valid-path? remote item)
          (info (G_ "'~a' successfully imported '~a'~%")
                name item)
          (leave (G_ "'~a' was not properly imported on '~a'~%")
                 item name)))))

(define (assert-node-can-export session node name daemon-socket)
  "Bail out if we cannot import signed archives from NODE."
  (let* ((remote  (connect-to-remote-daemon session daemon-socket))
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
           (sessions (map (cut open-ssh-session <> %short-timeout) machines))
           (nodes    (map remote-inferior sessions)))
      (for-each assert-node-has-guix nodes names)
      (for-each assert-node-repl nodes names)
      (for-each assert-node-can-import sessions nodes names sockets)
      (for-each assert-node-can-export sessions nodes names sockets)
      (for-each close-inferior nodes)
      (for-each disconnect! sessions))))

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
                (define session
                  (open-ssh-session machine %short-timeout))

                (match (remote-inferior session)
                  (#f
                   (warning (G_ "failed to run 'guix repl' on machine '~a'~%")
                            (build-machine-name machine)))
                  ((? inferior? inferior)
                   (let ((now (car (gettimeofday))))
                     (match (inferior-eval '(list (uname)
                                                  (car (gettimeofday)))
                                           inferior)
                       ((uts time)
                        (when (< time now)
                          ;; Build machine clocks must not be behind as this
                          ;; could cause timestamp issues.
                          (warning (G_ "machine '~a' is ~a seconds behind~%")
                                   (build-machine-name machine)
                                   (- now time)))

                        (let ((load (node-load inferior))
                              (free (node-free-disk-space inferior)))
                          (close-inferior inferior)
                          (format #t "~a~%  kernel: ~a ~a~%  architecture: ~a~%\
  host name: ~a~%  normalized load: ~,2f~%  free disk space: ~,2f MiB~%\
  time difference: ~a s~%"
                                  (build-machine-name machine)
                                  (utsname:sysname uts) (utsname:release uts)
                                  (utsname:machine uts)
                                  (utsname:nodename uts)
                                  load
                                  (/ free (expt 2 20) 1.)
                                  (- time now))))))))

                (disconnect! session))
              machines)))


;;;
;;; Entry point.
;;;

(define-command (guix-offload . args)
  (category plumbing)
  (synopsis "set up and operate build offloading")

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
                                        (match:substring match 3)
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
     (format #t (G_ "Usage: guix offload SYSTEM MAX-SILENT-TIME \
PRINT-BUILD-TRACE? BUILD-TIMEOUT
Process build offload requests written on the standard input, possibly
offloading builds to the machines listed in '~a'.~%")
             %machine-file)
     (display (G_ "
This tool is meant to be used internally by 'guix-daemon'.\n"))
     (show-bug-report-information))
    (x
     (leave (G_ "invalid arguments: ~{~s ~}~%") x))))

;;; Local Variables:
;;; eval: (put 'with-error-to-port 'scheme-indent-function 1)
;;; eval: (put 'with-timeout 'scheme-indent-function 2)
;;; End:

;;; offload.scm ends here
