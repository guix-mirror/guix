;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix config)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix nar)
  #:use-module (guix utils)
  #:use-module ((guix build utils) #:select (which mkdir-p))
  #:use-module (guix ui)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (rnrs io ports)
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
  (system          build-machine-system)          ; string
  (user            build-machine-user)            ; string
  (private-key     build-machine-private-key      ; file name
                   (default (user-lsh-private-key)))
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

(define %lsh-command
  "lsh")

(define %lshg-command
  ;; FIXME: 'lshg' fails to pass large amounts of data, see
  ;; <http://lists.lysator.liu.se/pipermail/lsh-bugs/2014q1/000639.html>.
  "lsh")

(define (user-lsh-private-key)
  "Return the user's default lsh private key, or #f if it could not be
determined."
  (and=> (getenv "HOME")
         (cut string-append <> "/.lsh/identity")))

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
      (set! %fresh-auto-compile #t)

      (save-module-excursion
       (lambda ()
         (set-current-module %user-module)
         (primitive-load file))))
    (lambda args
      (match args
        (('system-error . _)
         (let ((err (system-error-errno args)))
           ;; Silently ignore missing file since this is a common case.
           (if (= ENOENT err)
               '()
               (leave (_ "failed to open machine file '~a': ~a~%")
                      file (strerror err)))))
        (_
         (leave (_ "failed to load machine file '~a': ~s~%")
                file args))))))

;;; FIXME: The idea was to open the connection to MACHINE once for all, but
;;; lshg is currently non-functional.
;; (define (open-ssh-gateway machine)
;;   "Initiate an SSH connection gateway to MACHINE, and return the PID of the
;; running lsh gateway upon success, or #f on failure."
;;   (catch 'system-error
;;     (lambda ()
;;       (let* ((port   (open-pipe* OPEN_READ %lsh-command
;;                                  "-l" (build-machine-user machine)
;;                                  "-i" (build-machine-private-key machine)
;;                                  ;; XXX: With lsh 2.1, passing '--write-pid'
;;                                  ;; last causes the PID not to be printed.
;;                                  "--write-pid" "--gateway" "--background" "-z"
;;                                  (build-machine-name machine)))
;;              (line   (read-line port))
;;              (status (close-pipe port)))
;;        (if (zero? status)
;;            (let ((pid (string->number line)))
;;              (if (integer? pid)
;;                  pid
;;                  (begin
;;                    (warning (_ "'~a' did not write its PID on stdout: ~s~%")
;;                             %lsh-command line)
;;                    #f)))
;;            (begin
;;              (warning (_ "failed to initiate SSH connection to '~a':\
;;  '~a' exited with ~a~%")
;;                       (build-machine-name machine)
;;                       %lsh-command
;;                       (status:exit-val status))
;;              #f))))
;;     (lambda args
;;       (leave (_ "failed to execute '~a': ~a~%")
;;              %lsh-command (strerror (system-error-errno args))))))

(define (remote-pipe machine mode command)
  "Run COMMAND on MACHINE, assuming an lsh gateway has been set up."
  (catch 'system-error
    (lambda ()
      (apply open-pipe* mode %lshg-command
             "-l" (build-machine-user machine) "-z"

             ;; XXX: Remove '-i' when %LSHG-COMMAND really is lshg.
             "-i" (build-machine-private-key machine)

             (build-machine-name machine)
             command))
    (lambda args
      (warning (_ "failed to execute '~a': ~a~%")
               %lshg-command (strerror (system-error-errno args)))
      #f)))

(define* (offload drv machine
                  #:key print-build-trace? (max-silent-time 3600)
                  (build-timeout 7200) (log-port (current-output-port)))
  "Perform DRV on MACHINE, assuming DRV and its prerequisites are available
there, and write the build log to LOG-PORT.  Return the exit status."
  (format (current-error-port) "offloading '~a' to '~a'...~%"
          (derivation-file-name drv) (build-machine-name machine))
  (format (current-error-port) "@ build-remote ~a ~a~%"
          (derivation-file-name drv) (build-machine-name machine))

  ;; FIXME: Protect DRV from garbage collection on MACHINE.
  (let ((pipe (remote-pipe machine OPEN_READ
                           `("guix" "build"
                             ;; FIXME: more options
                             ,(format #f "--max-silent-time=~a"
                                      max-silent-time)
                             ,(derivation-file-name drv)))))
    (let loop ((line (read-line pipe)))
      (unless (eof-object? line)
        (display line log-port)
        (newline log-port)
        (loop (read-line pipe))))

    (close-pipe pipe)))

(define (send-files files machine)
  "Send the subset of FILES that's missing to MACHINE's store.  Return #t on
success, #f otherwise."
  (define (missing-files files)
    ;; Return the subset of FILES not already on MACHINE.
    (let* ((files   (format #f "~{~a~%~}" files))
           (missing (filtered-port
                     (list (which %lshg-command)
                           "-l" (build-machine-user machine)
                           "-i" (build-machine-private-key machine)
                           (build-machine-name machine)
                           "guix" "archive" "--missing")
                     (open-input-string files))))
      (string-tokenize (get-string-all missing))))

  (with-store store
    (guard (c ((nix-protocol-error? c)
               (warning (_ "failed to export files for '~a': ~s~%")
                        (build-machine-name machine)
                        c)
               (false-if-exception (close-pipe pipe))
               #f))

      ;; Compute the subset of FILES missing on MACHINE, and send them in
      ;; topologically sorted order so that they can actually be imported.
      (let ((files (missing-files (topologically-sorted store files)))
            (pipe  (remote-pipe machine OPEN_WRITE
                                '("guix" "archive" "--import"))))
        (format #t (_ "sending ~a store files to '~a'...~%")
                (length files) (build-machine-name machine))
        (catch 'system-error
          (lambda ()
            (export-paths store files pipe))
          (lambda args
            (warning (_ "failed while exporting files to '~a': ~a~%")
                     (build-machine-name machine)
                     (strerror (system-error-errno args)))))
        (zero? (close-pipe pipe))))))

(define (retrieve-files files machine)
  "Retrieve FILES from MACHINE's store, and import them."
  (define host
    (build-machine-name machine))

  (let ((pipe (remote-pipe machine OPEN_READ
                           `("guix" "archive" "--export" ,@files))))
    (and pipe
         (with-store store
           (guard (c ((nix-protocol-error? c)
                      (warning (_ "failed to import files from '~a': ~s~%")
                               host c)
                      #f))
             (format (current-error-port) "retrieving ~a files from '~a'...~%"
                     (length files) host)

             ;; We cannot use the 'import-paths' RPC here because we already
             ;; hold the locks for FILES.
             (restore-file-set pipe
                               #:log-port (current-error-port)
                               #:lock? #f)

             (zero? (close-pipe pipe)))))))

(define (machine-matches? machine requirements)
  "Return #t if MACHINE matches REQUIREMENTS."
  (and (string=? (build-requirements-system requirements)
                 (build-machine-system machine))
       (lset<= string=?
               (build-requirements-features requirements)
               (build-machine-features machine))))

(define (machine-faster? m1 m2)
  "Return #t if M1 is faster than M2."
  (> (build-machine-speed m1) (build-machine-speed m2)))

(define (machine-load machine)
  "Return the load of MACHINE, divided by the number of parallel builds
allowed on MACHINE."
  (let* ((pipe (remote-pipe machine OPEN_READ `("cat" "/proc/loadavg")))
         (line (read-line pipe)))
    (close-pipe pipe)
    (if (eof-object? line)
        1.
        (match (string-tokenize line)
          ((one five fifteen . _)
           (let* ((raw        (string->number five))
                  (jobs       (build-machine-parallel-builds machine))
                  (normalized (/ raw jobs)))
             (format (current-error-port) "load on machine '~a' is ~s\
 (normalized: ~s)~%"
                     (build-machine-name machine) raw normalized)
             normalized))
          (_
           1.)))))

(define (machine-less-loaded? m1 m2)
  "Return #t if the load on M1 is lower than that on M2."
  (< (machine-load m1) (machine-load m2)))

(define (machine-less-loaded-or-faster? m1 m2)
  "Return #t if M1 is either less loaded or faster than M2."
  (or (machine-less-loaded? m1 m2)
      (machine-faster? m1 m2)))

(define (machine-lock-file machine hint)
  "Return the name of MACHINE's lock file for HINT."
  (string-append %state-directory "/offload/"
                 (build-machine-name machine)
                 "." (symbol->string hint) ".lock"))

(define (lock-machine machine hint)
  "Wait to acquire MACHINE's lock for HINT, and return the lock."
  (let ((file (machine-lock-file machine hint)))
    (mkdir-p (dirname file))
    (let ((port (open-file file "w0")))
      (fcntl-flock port 'write-lock)
      port)))

(define (unlock-machine lock)
  "Unlock LOCK."
  (fcntl-flock lock 'unlock)
  (close-port lock)
  #t)

(define-syntax-rule (with-machine-lock machine hint exp ...)
  "Wait to acquire MACHINE's exclusive lock for HINT, and evaluate EXP in that
context."
  (let* ((m    machine)
         (lock (lock-machine m hint)))
    (dynamic-wind
      (lambda ()
        #t)
      (lambda ()
        exp ...)
      (lambda ()
        (unlock-machine lock)))))

(define (choose-build-machine requirements machines)
  "Return the best machine among MACHINES fulfilling REQUIREMENTS, or #f."
  (let ((machines (sort (filter (cut machine-matches? <> requirements)
                                machines)
                        machine-less-loaded-or-faster?)))
    (match machines
      ((head . _)
       ;; Return the best machine unless it's already overloaded.
       (and (< (machine-load head) 2.)
            head))
      (_ #f))))

(define* (process-request wants-local? system drv features
                          #:key
                          print-build-trace? (max-silent-time 3600)
                          (build-timeout 7200))
  "Process a request to build DRV."
  (let* ((local?  (and wants-local? (string=? system (%current-system))))
         (reqs    (build-requirements
                   (system system)
                   (features features)))
         (machine (choose-build-machine reqs (build-machines))))
    (if machine
        (begin
          (display "# accept\n")
          (let ((inputs  (string-tokenize (read-line)))
                (outputs (string-tokenize (read-line))))
            ;; Acquire MACHINE's exclusive lock to serialize file transfers
            ;; to/from MACHINE in the presence of several 'offload' hook
            ;; instance.
            (when (with-machine-lock machine 'bandwidth
                    (send-files (cons (derivation-file-name drv) inputs)
                                machine))
              (let ((status (offload drv machine
                                     #:print-build-trace? print-build-trace?
                                     #:max-silent-time max-silent-time
                                     #:build-timeout build-timeout)))
                (if (zero? status)
                    (begin
                      ;; Likewise (see above.)
                      (with-machine-lock machine 'bandwidth
                        (retrieve-files outputs machine))
                      (format (current-error-port)
                              "done with offloaded '~a'~%"
                              (derivation-file-name drv)))
                    (begin
                      (format (current-error-port)
                              "derivation '~a' offloaded to '~a' failed \
with exit code ~a~%"
                              (derivation-file-name drv)
                              (build-machine-name machine)
                              (status:exit-val status))
                      (primitive-exit (status:exit-val status))))))))
        (display "# decline\n"))))

(define-syntax-rule (with-nar-error-handling body ...)
  "Execute BODY with any &nar-error suitably reported to the user."
  (guard (c ((nar-error? c)
             (let ((file (nar-error-file c)))
               (if (condition-has-type? c &message)
                   (leave (_ "while importing file '~a': ~a~%")
                          file (gettext (condition-message c)))
                   (leave (_ "failed to import file '~a'~%")
                          file)))))
    body ...))


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

  (match args
    ((system max-silent-time print-build-trace? build-timeout)
     (let ((max-silent-time    (string->number max-silent-time))
           (build-timeout      (string->number build-timeout))
           (print-build-trace? (string=? print-build-trace? "1")))
       (parameterize ((%current-system system))
         (let loop ((line (read-line)))
           (unless (eof-object? line)
             (cond ((regexp-exec request-line-rx line)
                    =>
                    (lambda (match)
                      (with-nar-error-handling
                       (process-request (equal? (match:substring match 1) "1")
                                        (match:substring match 2) ; system
                                        (call-with-input-file
                                            (match:substring match 3)
                                          read-derivation)
                                        (string-tokenize
                                         (match:substring match 4) not-coma)
                                        #:print-build-trace? print-build-trace?
                                        #:max-silent-time max-silent-time
                                        #:build-timeout build-timeout))))
                   (else
                    (leave (_ "invalid request line: ~s~%") line)))
             (loop (read-line)))))))
    (("--version")
     (show-version-and-exit "guix offload"))
    (("--help")
     (format #t (_ "Usage: guix offload SYSTEM PRINT-BUILD-TRACE
Process build offload requests written on the standard input, possibly
offloading builds to the machines listed in '~a'.~%")
             %machine-file)
     (display (_ "
This tool is meant to be used internally by 'guix-daemon'.\n"))
     (show-bug-report-information))
    (x
     (leave (_ "invalid arguments: ~{~s ~}~%") x))))

;;; Local Variables:
;;; eval: (put 'with-machine-lock 'scheme-indent-function 2)
;;; End:

;;; offload.scm ends here
